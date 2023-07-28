open Core
open Func
open Source

let ( let* ) o f = Result.bind o ~f
let ( let+ ) o f = Result.map o ~f
let return x = Result.Ok x

let list_map ~f l =
  let exception E of string in
  try
    return
    @@ List.map l ~f:(fun v ->
           match f v with Error s -> raise (E s) | Ok v -> v)
  with E s -> Error s

module Crash = Err.Make ()
module Invalid_arg = Err.Make ()

exception Crash = Crash.Error
exception Invalid_arg = Invalid_arg.Error

module Make (P : Eval_functor_intf.P) :
  Eval_functor_intf.S with type env := P.env and type value = P.value = struct
  module Value = P.Value
  module Extern_func = P.Extern_func
  module Store = P.Store
  module Object = P.Object
  module Heap = P.Heap
  module Env = P.Env
  module Choice = P.Choice
  module Reducer = P.Reducer
  module Translator = P.Translator

  type value = P.value
  type store = P.store

  module State = struct
    module ESet = Set.Make (Encoding.Expression)

    type store = Store.t
    type nonrec env = P.env
    type solver = Batch.t
    type optimizer = Encoding.Optimizer.t

    type symb_env = {
      solver : Batch.t;
      (* optimizer : Encoding.Optimizer.t; *)
      path_condition : ESet.t;
    }

    type exec_state = {
      return_state : (exec_state * string) option;
      locals : store;
      stmts : Stmt.t list;
      env : env;
      func : string;
      symb_env : symb_env;
    }

    let empty_state ~env =
      let solver =
        let s = Batch.create () in
        if !Config.axioms then Batch.add s Encoding.Axioms.axioms;
        s
      in
      {
        return_state = None;
        locals = Store.create [];
        stmts = [];
        env;
        func = "";
        symb_env =
          {
            path_condition = ESet.empty;
            solver;
            (* optimizer = Encoding.Optimizer.create (); *)
          };
      }

    type stmt_result = Return of exec_state | Continue of exec_state

    type _stmt_err =
      | Error of string
      | Assertion of value list
      | Unknown of value list

    let ret (state : exec_state) (v : value) : stmt_result =
      match state.return_state with
      | None -> Return state
      | Some (state', ret_v) ->
          let locals = Store.add_exn state'.locals ret_v v in
          let env = state.env in
          let symb_env = state.symb_env in
          Continue { state' with locals; env; symb_env }
  end

  let eval_reduce_expr (sto : store) (e : Expr.t) : (value, string) Result.t =
    let+ e' = Value.eval_expr sto e in
    Reducer.reduce e'

  let pp locals e =
    match eval_reduce_expr locals e with
    | Ok v -> Value.Pp.pp v
    | Error msg ->
        Format.printf "%s@." msg;
        assert false

  let exec_func state func args ret_var =
    Log.debug (lazy (sprintf "calling func: %s" func.name));
    let return_state = Some (state, ret_var) in
    let params = func.params in
    let store = Store.create (List.zip_exn params args) in
    let state' =
      State.
        {
          return_state;
          locals = store;
          stmts = [ func.body ];
          env = state.env;
          func = func.name;
          symb_env = state.symb_env;
        }
    in
    return [ State.Continue state' ]

  let exec_extern_func state f args ret_var =
    let open Extern_func in
    let rec apply : type a. value Stack.t -> a Extern_func.atype -> a -> value =
     fun args ty f ->
      match ty with
      | UArg ty' -> apply args ty' (f ())
      | Arg ty' ->
          let v = Stack.pop_exn args in
          apply args ty' (f v)
      | Res -> f
    in
    let (Extern_func (Func atype, func)) = f in
    let v = apply (Stack.of_list args) atype func in
    let locals = Store.add_exn state.State.locals ret_var v in
    return [ State.Continue State.{ state with locals } ]

  let exec_stmt stmt (c : State.exec_state) :
      (State.stmt_result list, string) Result.t =
    let open State in
    let { locals; env; symb_env; _ } = c in
    let st store = return [ State.Continue { c with locals = store } ] in
    Log.debug
      (lazy (sprintf "store       : %s" (Value.Pp.Store.to_string locals)));
    Log.debug
      (lazy (sprintf "running stmt: %s" (Stmt.Pp.to_string stmt (pp locals))));
    match stmt.it with
    | Stmt.Skip -> st locals
    | Stmt.Merge -> st locals
    | Stmt.Exception err ->
        let at' = Source.string_of_region stmt.at in
        Error (sprintf "%s: Exception: %s" at' err)
    | Stmt.Fail e ->
        let* e' = eval_reduce_expr locals e in
        Error (sprintf "fail: %s" (Value.Pp.pp e'))
    | Stmt.Abort e ->
        let* e' = eval_reduce_expr locals e in
        Error (sprintf "abort: %s" (Value.Pp.pp e'))
    | Stmt.Print e ->
        let* e' = eval_reduce_expr locals e in
        (* let s = *)
        (*   match e' with *)
        (*   | Expr.Val (Val.Loc l) -> *)
        (*       let heap = Env.get_memory env in *)
        (*       let o = Heap.get heap l in *)
        (*       Object.to_string (Option.value_exn o) Expr.str *)
        (*   | _ -> Expr.str e' *)
        (* in *)
        (* (1* Printf.printf "print:%s\npc:%s\nheap id:%d\n" s (Encoding.Expression.string_of_pc pc) (Heap.get_id heap); *1) *)
        Format.printf "%s@." (Value.Pp.pp e');
        st locals
    | Stmt.Assign (x, e) ->
        let* v = eval_reduce_expr locals e in
        st @@ Store.add_exn locals x v
    | Stmt.Assert e ->
        let* e' = eval_reduce_expr locals e in
        let pc = ESet.to_list symb_env.path_condition in
        let b = Choice.assertion symb_env.solver pc e' in
        if b then st locals else Error (sprintf "assert: %s" (Value.Pp.pp e'))
    | Stmt.Block blk ->
        return [ State.Continue { c with stmts = blk @ c.stmts } ]
    | Stmt.If (br, blk1, blk2) ->
        let* br' = eval_reduce_expr locals br in
        let pc = ESet.to_list symb_env.path_condition in
        let (t_branch, t_cond), (f_branch, f_cond) =
          Choice.branch symb_env.solver pc br'
        in
        let states =
          if not t_branch then []
          else
            let pc' =
              Option.fold t_cond ~init:symb_env.path_condition ~f:ESet.add
            in
            let symb_env = { symb_env with path_condition = pc' } in
            let stmts = blk1 :: c.stmts in
            [ { c with stmts; symb_env } ]
        in
        let states =
          if not f_branch then states
          else
            let pc' =
              Option.fold f_cond ~init:symb_env.path_condition ~f:ESet.add
            in
            let symb_env = { symb_env with path_condition = pc' } in
            let stmts = Option.fold blk2 ~init:c.stmts ~f:(fun a b -> b :: a) in
            { c with stmts; symb_env } :: states
        in
        return
          (match states with
          | [] -> []
          | [ state ] -> [ State.Continue state ]
          | _ ->
              List.map states ~f:(fun c ->
                  let env = Env.clone c.env in
                  State.Continue { c with env }))
    | Stmt.While (br, blk) ->
        let blk' =
          Stmt.Block (blk :: [ Stmt.While (br, blk) @> stmt.at ]) @> blk.at
        in
        let stmts = (Stmt.If (br, blk', None) @> stmt.at) :: c.stmts in
        return [ State.Continue { c with stmts } ]
    | Stmt.Return e ->
        let* v = eval_reduce_expr locals e in
        return [ State.ret c v ]
    | Stmt.AssignCall (x, f, es) ->
        let* f' = eval_reduce_expr locals f in
        let* func_name, args0 = Value.get_func_name f' in
        let* func = Env.get_func env func_name in
        let* args = list_map ~f:(eval_reduce_expr locals) es in
        let args = args0 @ args in
        exec_func c func args x
    | Stmt.AssignECall (x, f, es) ->
        let* func = Env.get_extern_func env f in
        let* args = list_map ~f:(eval_reduce_expr locals) es in
        exec_extern_func c func args x
    | Stmt.AssignNewObj x ->
        let heap = Env.get_memory env in
        let obj = Object.create () in
        let loc = Heap.insert heap obj in
        st @@ Store.add_exn locals x loc
    | Stmt.AssignInObjCheck (_x, _e_field, _e_loc) ->
        assert false
        (* let* e_loc' = eval_reduce_expr locals e_loc in *)
        (* let* locs = Expr.loc e_loc' in *)
        (* let* field = eval_reduce_expr locals e_field in *)
        (* return *)
        (*   (List.fold locs ~init:[] ~f:(fun accum (cond, l) -> *)
        (*        match cond with *)
        (*        | None -> *)
        (*            let env = *)
        (*              if List.length locs > 1 then Env.clone env else env *)
        (*            in *)
        (*            let heap = Env.get_memory env in *)
        (*            let v = Heap.has_field heap l field in *)
        (*            let locals = Store.add_exn locals x v in *)
        (*            State.Continue { c with locals; env } :: accum *)
        (*        | Some cond' -> *)
        (*            let pc' = *)
        (*              ESet.add symb_env.path_condition *)
        (*                (Translator.translate cond') *)
        (*            in *)
        (*            if not (Batch.check symb_env.solver (ESet.to_list pc')) then *)
        (*              accum *)
        (*            else *)
        (*              let env = Env.clone env in *)
        (*              let heap = Env.get_memory env in *)
        (*              let v = Heap.has_field heap l field in *)
        (*              let locals = Store.add_exn locals x v in *)
        (*              let symb_env = { symb_env with path_condition = pc' } in *)
        (*              State.Continue { c with locals; env; symb_env } :: accum)) *)
    | Stmt.AssignObjToList (_x, _e) ->
        assert false
        (* let f env l pc' = *)
        (*   let v = *)
        (*     let h = Env.get_memory env in *)
        (*     match Heap.get h l with *)
        (*     | None -> Crash.error stmt.at ("'" ^ l ^ "' not found in heap") *)
        (*     | Some obj -> *)
        (*         Expr.NOpt *)
        (*           ( Operators.ListExpr, *)
        (*             List.map (Object.to_list obj) ~f:(fun (f, v) -> *)
        (*                 Expr.NOpt (Operators.TupleExpr, [ f; v ])) ) *)
        (*   in *)
        (*   let locals = Store.add_exn locals x v in *)
        (*   let symb_env = { symb_env with path_condition = pc' } in *)
        (*   State.Continue { c with locals; env; symb_env } *)
        (* in *)
        (* let* e' = eval_reduce_expr locals e in *)
        (* let* locs = Expr.loc e' in *)
        (* return *)
        (*   (List.fold locs ~init:[] ~f:(fun accum (cond, l) -> *)
        (*        match cond with *)
        (*        | None -> *)
        (*            let env' = *)
        (*              if List.length locs > 1 then Env.clone env else env *)
        (*            in *)
        (*            f env' l symb_env.path_condition :: accum *)
        (*        | Some cond' -> *)
        (*            let pc' = *)
        (*              ESet.add symb_env.path_condition *)
        (*                (Translator.translate cond') *)
        (*            in *)
        (*            if not (Batch.check symb_env.solver (ESet.to_list pc')) then *)
        (*              accum *)
        (*            else f (Env.clone env) l pc' :: accum)) *)
    | Stmt.AssignObjFields (_x, _e) ->
        assert false
        (* let f env l pc' = *)
        (*   let v = *)
        (*     let h = Env.get_memory env in *)
        (*     match Heap.get h l with *)
        (*     | None -> Crash.error stmt.at ("'" ^ l ^ "' not found in heap") *)
        (*     | Some obj -> Expr.NOpt (Operators.ListExpr, Object.get_fields obj) *)
        (*   in *)
        (*   let locals = Store.add_exn locals x v in *)
        (*   let symb_env = { symb_env with path_condition = pc' } in *)
        (*   State.Continue { c with locals; env; symb_env } *)
        (* in *)
        (* let* e' = eval_reduce_expr locals e in *)
        (* let* locs = Expr.loc e' in *)
        (* return *)
        (*   (List.fold locs ~init:[] ~f:(fun accum (cond, l) -> *)
        (*        match cond with *)
        (*        | None -> *)
        (*            let env' = *)
        (*              if List.length locs > 1 then Env.clone env else env *)
        (*            in *)
        (*            f env' l symb_env.path_condition :: accum *)
        (*        | Some cond' -> *)
        (*            let pc' = *)
        (*              ESet.add symb_env.path_condition *)
        (*                (Translator.translate cond') *)
        (*            in *)
        (*            if not (Batch.check symb_env.solver (ESet.to_list pc')) then *)
        (*              accum *)
        (*            else f (Env.clone env) l pc' :: accum)) *)
    | Stmt.FieldAssign (_e_loc, _e_field, _e_v) ->
        assert false
        (* let* e_loc' = eval_reduce_expr locals e_loc in *)
        (* let* locs = Expr.loc e_loc' in *)
        (* let* reduced_field = eval_reduce_expr locals e_field in *)
        (* let* v = eval_reduce_expr locals e_v in *)
        (* return *)
        (*   (List.fold locs ~init:[] ~f:(fun accum (cond, l) -> *)
        (*        match cond with *)
        (*        | None -> *)
        (*            let heap = Env.get_memory env in *)
        (*            let heap' = *)
        (*              if List.length locs > 1 then Heap.clone heap else heap *)
        (*            in *)
        (*            let pc = symb_env.path_condition in *)
        (*            let objects = *)
        (*              Heap.set_field heap' l reduced_field v symb_env.solver *)
        (*                (ESet.to_list pc) *)
        (*            in *)
        (*            List.map objects ~f:(fun (new_heap, new_pc) -> *)
        (*                let pc' = List.fold new_pc ~init:pc ~f:ESet.add in *)
        (*                let env = Env.add_memory env new_heap in *)
        (*                let symb_env = { symb_env with path_condition = pc' } in *)
        (*                State.Continue { c with env; symb_env }) *)
        (*        | Some cond' -> *)
        (*            let pc = symb_env.path_condition in *)
        (*            let pc' = ESet.add pc (Translator.translate cond') in *)
        (*            if not (Batch.check symb_env.solver (ESet.to_list pc')) then *)
        (*              accum *)
        (*            else *)
        (*              let heap = Env.get_memory env in *)
        (*              let objects = *)
        (*                Heap.set_field heap l reduced_field v symb_env.solver *)
        (*                  (ESet.to_list pc) *)
        (*              in *)
        (*              List.map objects ~f:(fun (new_heap, new_pc) -> *)
        (*                  let pc' = List.fold new_pc ~init:pc ~f:ESet.add in *)
        (*                  let env = Env.add_memory env new_heap in *)
        (*                  let symb_env = *)
        (*                    { symb_env with path_condition = pc' } *)
        (*                  in *)
        (*                  State.Continue { c with env; symb_env }))) *)
    | Stmt.FieldDelete (_e_loc, _e_field) ->
        assert false
        (* let* e_loc' = eval_reduce_expr locals e_loc in *)
        (* let* locs = Expr.loc e_loc' in *)
        (* let* reduced_field = eval_reduce_expr locals e_field in *)
        (* return *)
        (*   (List.fold locs ~init:[] ~f:(fun accum (cond, l) -> *)
        (*        match cond with *)
        (*        | None -> *)
        (*            let heap = Env.get_memory env in *)
        (*            let heap' = *)
        (*              if List.length locs > 1 then Heap.clone heap else heap *)
        (*            in *)
        (*            let pc = symb_env.path_condition in *)
        (*            let objects = *)
        (*              Heap.delete_field heap' l reduced_field symb_env.solver *)
        (*                (ESet.to_list pc) *)
        (*            in *)
        (*            List.map objects ~f:(fun (new_heap, new_pc) -> *)
        (*                let pc' = List.fold new_pc ~init:pc ~f:ESet.add in *)
        (*                let env = Env.add_memory env new_heap in *)
        (*                let symb_env = { symb_env with path_condition = pc' } in *)
        (*                State.Continue { c with env; symb_env }) *)
        (*        | Some cond' -> *)
        (*            let pc = symb_env.path_condition in *)
        (*            let pc' = ESet.add pc (Translator.translate cond') in *)
        (*            if not (Batch.check symb_env.solver (ESet.to_list pc')) then *)
        (*              accum *)
        (*            else *)
        (*              let heap = Env.get_memory env in *)
        (*              let objects = *)
        (*                Heap.delete_field heap l reduced_field symb_env.solver *)
        (*                  (ESet.to_list pc) *)
        (*              in *)
        (*              List.map objects ~f:(fun (new_heap, new_pc) -> *)
        (*                  let pc' = List.fold new_pc ~init:pc ~f:ESet.add in *)
        (*                  let env = Env.add_memory env new_heap in *)
        (*                  let symb_env = *)
        (*                    { symb_env with path_condition = pc' } *)
        (*                  in *)
        (*                  State.Continue { c with env; symb_env }))) *)
    | Stmt.FieldLookup (_x, _e_loc, _e_field) ->
        assert false
        (* let* e_loc' = eval_reduce_expr locals e_loc in *)
        (* let* locs = Expr.loc e_loc' in *)
        (* let* reduced_field = eval_reduce_expr locals e_field in *)
        (* return *)
        (*   (List.fold locs ~init:[] ~f:(fun accum (cond, l) -> *)
        (*        match cond with *)
        (*        | None -> *)
        (*            let heap = Env.get_memory env in *)
        (*            let heap' = *)
        (*              if List.length locs > 1 then Heap.clone heap else heap *)
        (*            in *)
        (*            let pc = symb_env.path_condition in *)
        (*            let objects = *)
        (*              Heap.get_field heap' l reduced_field symb_env.solver *)
        (*                (ESet.to_list pc) *)
        (*            in *)
        (*            List.map objects ~f:(fun (new_heap, new_pc, v) -> *)
        (*                let v' = *)
        (*                  Option.value v *)
        (*                    ~default:(Expr.Val (Val.Symbol "undefined")) *)
        (*                in *)
        (*                let pc' = List.fold new_pc ~init:pc ~f:ESet.add in *)
        (*                let locals = Store.add_exn locals x v' in *)
        (*                let env = Env.add_memory env new_heap in *)
        (*                let symb_env = { symb_env with path_condition = pc' } in *)
        (*                State.Continue { c with locals; env; symb_env }) *)
        (*        | Some cond' -> *)
        (*            let pc = symb_env.path_condition in *)
        (*            let pc' = ESet.add pc (Translator.translate cond') in *)
        (*            if not (Batch.check symb_env.solver (ESet.to_list pc')) then *)
        (*              accum *)
        (*            else *)
        (*              let heap = Env.get_memory env in *)
        (*              let objects = *)
        (*                Heap.get_field heap l reduced_field symb_env.solver *)
        (*                  (ESet.to_list pc) *)
        (*              in *)
        (*              List.map objects ~f:(fun (new_heap, new_pc, v) -> *)
        (*                  let v' = *)
        (*                    Option.value v *)
        (*                      ~default:(Expr.Val (Val.Symbol "undefined")) *)
        (*                  in *)
        (*                  let pc' = List.fold new_pc ~init:pc ~f:ESet.add in *)
        (*                  let locals = Store.add_exn locals x v' in *)
        (*                  let env = Env.add_memory env new_heap in *)
        (*                  let symb_env = *)
        (*                    { symb_env with path_condition = pc' } *)
        (*                  in *)
        (*                  State.Continue { c with locals; env; symb_env }))) *)
    (* To deprecate *)
    | Stmt.SymStmt (SymStmt.Assume e) -> (
        let* e' = eval_reduce_expr locals e in
        (* TODO: Do not discharge to solver (saves 1 query per assume) *)
        match Choice.assumption e' with
        | Some b -> if b then st locals else return []
        | None ->
            let pc' =
              ESet.add symb_env.path_condition (Translator.translate e')
            in
            let symb_env = { symb_env with path_condition = pc' } in
            return [ State.Continue { c with symb_env } ])
    | Stmt.SymStmt (SymStmt.Evaluate (_x, _e)) ->
        assert false
        (* let* e' = eval_reduce_expr locals e in *)
        (* let e' = Translator.translate e' in *)
        (* let _sym_e = List.hd (Encoding.Expression.get_symbols [ e' ]) in *)
        (* assert ( *)
        (*   Batch.check symb_env.solver (ESet.to_list symb_env.path_condition)); *)
        (* assert false *)
    | Stmt.SymStmt (SymStmt.Maximize (_x, _e)) ->
        assert false
        (* let* e' = eval_reduce_expr locals e in *)
        (* let e' = Translator.translate e' in *)
        (* let pc = ESet.to_list symb_env.path_condition in *)
        (* let v = *)
        (*   Option.map ~f:Translator.expr_of_value *)
        (*     (Encoding.Optimizer.maximize symb_env.optimizer e' pc) *)
        (* in *)
        (* st *)
        (* @@ Store.add_exn locals x (Option.value ~default:(Expr.Val Val.Null) v) *)
    | Stmt.SymStmt (SymStmt.Minimize (_x, _e)) ->
        assert false
        (* let* e' = eval_reduce_expr locals e in *)
        (* let e' = Translator.translate e' in *)
        (* let pc = ESet.to_list symb_env.path_condition in *)
        (* let v = *)
        (*   Option.map ~f:Translator.expr_of_value *)
        (*     (Encoding.Optimizer.minimize symb_env.optimizer e' pc) *)
        (* in *)
        (* st *)
        (* @@ Store.add_exn locals x (Option.value ~default:(Expr.Val Val.Null) v) *)
       (* st @@ Store.add_exn locals x (Expr.Bool.const (Expr.is_symbolic e')) *)
    | Stmt.SymStmt (SymStmt.Is_sat (_x, _e)) ->
        assert false
        (* let* e' = eval_reduce_expr locals e in *)
        (* let pc' = ESet.add symb_env.path_condition (Translator.translate e') in *)
        (* let sat = Batch.check symb_env.solver (ESet.to_list pc') in *)
        (* st @@ Store.add_exn locals x (Expr.Bool.const sat) *)
    (* Can remove *)
    | Stmt.SymStmt (SymStmt.Is_symbolic (_x, _e)) ->
        assert false
        (* let* e' = eval_reduce_expr locals e in *)
    | Stmt.SymStmt (SymStmt.Is_number (_x, _e)) -> assert false
  (* let* e' = eval_reduce_expr locals e in *)
  (* let is_num = *)
  (*   match Sval_typing.type_of e' with *)
  (*   | Some Type.IntType | Some Type.FltType -> true *)
  (*   | _ -> false *)
  (* in *)
  (* st @@ Store.add_exn locals x (Value.Bool.const is_num) *)

  module type WorkList = sig
    type 'a t

    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
    val length : 'a t -> int
  end

  let serialize =
    let open State in
    let counter = ref 0 in
    fun ?(witness : string option) (state : State.exec_state) ->
      let pc = State.ESet.to_list state.symb_env.path_condition in
      assert (Batch.check state.symb_env.solver pc);
      let model = Batch.model state.symb_env.solver in
      let testcase =
        Option.value_map model ~default:"[]" ~f:(fun m ->
            let open Encoding in
            let inputs =
              List.map (Model.get_bindings m) ~f:(fun (s, v) ->
                  let sort = Types.string_of_type (Symbol.type_of s) in
                  let name = Symbol.to_string s in
                  let interp = Value.to_string v in
                  sprintf
                    "{ \"type\" : \"%s\", \"name\" : \"%s\", \"value\" : \
                     \"%s\" }"
                    sort name interp)
            in
            String.concat ~sep:", " inputs)
      in
      let str_pc = Encoding.Expression.string_of_pc pc in
      let smt_query = Encoding.Expression.to_smt pc in
      let prefix =
        incr counter;
        let fname = if Option.is_some witness then "witness" else "testecase" in
        let fname = sprintf "%s-%i" fname !counter in
        Filename.concat (Filename.concat !Config.workspace "test-suite") fname
      in
      Io.write_file ~file:(sprintf "%s.json" prefix) ~data:testcase;
      Io.write_file ~file:(sprintf "%s.pc" prefix) ~data:str_pc;
      Io.write_file ~file:(sprintf "%s.smt2" prefix) ~data:smt_query;
      Option.iter witness ~f:(fun sink ->
          Io.write_file ~file:(sprintf "%s_sink.txt" prefix) ~data:sink)

  (* Source: Thanks to Joao Borges (@RageKnify) for writing this code *)
  module TreeSearch (L : WorkList) = struct
    open State

    let eval c =
      let time = ref (Stdlib.Sys.time ()) in
      let w = L.create () in
      L.push c w;
      while not (L.is_empty w) do
        let c = L.pop w in
        match c.stmts with
        | stmt :: stmts -> (
            let states = exec_stmt stmt { c with stmts } in
            match states with
            | Ok results ->
                List.iter results ~f:(fun result ->
                    match result with
                    | State.Continue state -> L.push state w
                    | State.Return state -> serialize state)
            | Error msg ->
                serialize ~witness:msg c;
                Log.debug (lazy (sprintf "error       : %s: %s" c.func msg)))
        | [] ->
            Format.printf "Empty continuation!@.";
            assert false
      done;
      time := Stdlib.Sys.time () -. !time;
      Format.printf "  exec time : %fs@." !time;
      Format.printf "solver time : %fs@." !Batch.solver_time;
      Format.printf "  mean time : %fms@."
        (1000. *. !Batch.solver_time /. float !Batch.solver_count)
  end

  (* Source: Thanks to Joao Borges (@RageKnify) for writing this code *)
  module RandArray : WorkList = struct
    type 'a t = 'a BatDynArray.t

    exception Empty

    let create () = BatDynArray.create ()
    let is_empty a = BatDynArray.empty a
    let push v a = BatDynArray.add a v

    let pop a =
      let i = Random.int (BatDynArray.length a) in
      let v = BatDynArray.get a i in
      BatDynArray.delete a i;
      v

    let length = BatDynArray.length
  end

  module DFS = TreeSearch (Stdlib.Stack)
  module BFS = TreeSearch (Stdlib.Queue)
  module RND = TreeSearch (RandArray)

  let analyse (env : Env.t) (f : string) (policy : string) =
    let f =
      match Env.get_func env f with Ok f -> f | Error msg -> failwith msg
    in
    let eval =
      match policy with
      | "breadth" -> BFS.eval
      | "depth" -> DFS.eval
      | "random" -> RND.eval
      | _ ->
          Crash.error f.body.at
            ("Invalid search policy '" ^ !Config.policy ^ "'")
    in
    let state = State.empty_state ~env in
    eval State.{ state with stmts = [ f.body ]; func = f.name }

  let main (env : Env.t) (f : string) : unit =
    let testsuite_path = Filename.concat !Config.workspace "test-suite" in
    Io.safe_mkdir testsuite_path;
    analyse env f !Config.policy
end
