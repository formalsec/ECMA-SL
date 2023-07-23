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
  Eval_functor_intf.S with type env := P.env and type value = Expr.t = struct
  module Store = P.Store
  module Object = P.Object
  module Heap = P.Heap
  module Reducer = P.Reducer
  module Env = P.Env

  type value = Expr.t

  module State = struct
    module ESet = Set.Make (Encoding.Expression)

    type store = Store.t
    type nonrec env = P.env
    type solver = Batch.t
    type optimizer = Encoding.Optimizer.t

    type symb_env = {
      solver : Batch.t;
      optimizer : Encoding.Optimizer.t;
      path_condition : ESet.t;
    }

    type exec_state = {
      return_state : (exec_state * string) option;
      locals : Store.t;
      stmts : Stmt.t list;
      env : env;
      func : string;
      symb_env : symb_env;
    }

    let empty_state ~env ~func =
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
        func;
        symb_env =
          {
            path_condition = ESet.empty;
            solver;
            optimizer = Encoding.Optimizer.create ();
          };
      }

    type stmt_result = Return of value | Continue of exec_state

    type _stmt_err =
      | Error of string
      | Assertion of value list
      | Unknown of value list

    let ret (state : exec_state) (v : value) : stmt_result =
      match state.return_state with
      | None -> Return v
      | Some (state', ret_v) ->
          let locals = Store.add_exn state'.locals ret_v v in
          let env = state.env in
          let symb_env = state.symb_env in
          Continue { state' with locals; env; symb_env }
  end

  (* Eval pass to remove variables from store *)
  let rec eval_expr (store : Store.t) (e : value) : (value, string) Result.t =
    match e with
    | Expr.Val v -> return (Expr.Val v)
    | Expr.Var x ->
        Result.of_option (Store.find store x)
          ~error:(sprintf "Cannot find var '%s'" x)
    | Expr.UnOpt (op, e) ->
        let+ e' = eval_expr store e in
        Expr.UnOpt (op, e')
    | Expr.BinOpt (op, e1, e2) ->
        let* e1' = eval_expr store e1 in
        let+ e2' = eval_expr store e2 in
        Expr.BinOpt (op, e1', e2')
    | Expr.TriOpt (op, e1, e2, e3) ->
        let* e1' = eval_expr store e1 in
        let* e2' = eval_expr store e2 in
        let+ e3' = eval_expr store e3 in
        Expr.TriOpt (op, e1', e2', e3')
    | Expr.NOpt (op, es) ->
        let+ es' = list_map ~f:(eval_expr store) es in
        Expr.NOpt (op, es')
    | Expr.Curry (f, es) ->
        let+ es' = list_map ~f:(eval_expr store) es in
        Expr.Curry (f, es')
    | Expr.Symbolic (t, x) ->
        let+ x' = eval_expr store x in
        Expr.Symbolic (t, x')

  let eval_reduce_expr (sto : Store.t) (e : value) : (value, string) Result.t =
    let+ e' = eval_expr sto e in
    Reducer.reduce_expr e'

  let block b =
    match b.it with
    | Stmt.Block b -> Ok b
    | _ -> Error "Malformed block statement"

  let pp locals e =
    match eval_reduce_expr locals e with
    | Ok v -> Expr.Pp.str v
    | Error _ -> assert false

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
    let rec apply : type a. Expr.t Stack.t -> a Extern_func.atype -> a -> Expr.t
        =
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
    Log.debug (lazy (sprintf "store       : %s" (Store.to_string locals)));
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
        Error (sprintf "fail: %s" (Expr.str e'))
    | Stmt.Abort e ->
        let* e' = eval_reduce_expr locals e in
        Error (sprintf "abort: %s" (Expr.str e'))
    | Stmt.Print e ->
        let* e' = eval_reduce_expr locals e in
        let s =
          match e' with
          | Expr.Val (Val.Loc l) ->
              let heap = Env.get_memory env in
              let o = Heap.get heap l in
              Object.to_string (Option.value_exn o) Expr.str
          | _ -> Expr.str e'
        in
        (* Printf.printf "print:%s\npc:%s\nheap id:%d\n" s (Encoding.Expression.string_of_pc pc) (Heap.get_id heap); *)
        Format.printf "%s@." s;
        st locals
    | Stmt.Assign (x, e) ->
        let* v = eval_reduce_expr locals e in
        st @@ Store.add_exn locals x v
    | Stmt.Assert e -> (
        let* e' = eval_reduce_expr locals e in
        match e' with
        | Expr.Val (Val.Bool b) ->
            if b then st locals else Error (sprintf "assert: %s" (Expr.str e'))
        | v ->
            let v' = Reducer.reduce_expr (Expr.Bool.not_ v) in
            let pc' =
              ESet.add symb_env.path_condition (Translator.translate v')
            in
            if Batch.check symb_env.solver (ESet.to_list pc') then
              Error (sprintf "assert: %s" (Expr.str v))
            else st locals)
    | Stmt.Block blk ->
        return [ State.Continue { c with stmts = blk @ c.stmts } ]
    | Stmt.If (br, blk1, blk2) -> (
        let* br' = eval_reduce_expr locals br in
        match br' with
        | Expr.Val (Val.Bool b) ->
            if b then
              let* b = block blk1 in
              return [ State.Continue { c with stmts = b @ c.stmts } ]
            else
              let stmts =
                Option.fold blk2 ~init:c.stmts ~f:(fun accum stmt ->
                    stmt :: accum)
              in
              return [ State.Continue { c with stmts } ]
        | cond ->
            let no = Reducer.reduce_expr (Expr.Bool.not_ cond) in
            let cond' = Translator.translate cond in
            let no' = Translator.translate no in
            let then_branch =
              let pc' = ESet.add symb_env.path_condition cond' in
              if not (Batch.check symb_env.solver (ESet.to_list pc')) then []
              else
                let env = Env.clone env in
                let symb_env = { symb_env with path_condition = pc' } in
                let stmts = blk1 :: c.stmts in
                [ State.Continue { c with stmts; env; symb_env } ]
            in
            let else_branch =
              let pc' = ESet.add symb_env.path_condition no' in
              if not (Batch.check symb_env.solver (ESet.to_list pc')) then []
              else
                let env = Env.clone env in
                let symb_env = { symb_env with path_condition = pc' } in
                let stmts =
                  match blk2 with None -> c.stmts | Some s' -> s' :: c.stmts
                in
                [ State.Continue { c with stmts; env; symb_env } ]
            in
            return (else_branch @ then_branch))
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
        let* func_name, args0 = Expr.func f' in
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
        st @@ Store.add_exn locals x (Expr.Val (Val.Loc loc))
    | Stmt.AssignInObjCheck (x, e_field, e_loc) ->
        let* e_loc' = eval_reduce_expr locals e_loc in
        let* locs = Expr.loc e_loc' in
        let* field = eval_reduce_expr locals e_field in
        return
          (List.fold locs ~init:[] ~f:(fun accum (cond, l) ->
               match cond with
               | None ->
                   let env =
                     if List.length locs > 1 then Env.clone env else env
                   in
                   let heap = Env.get_memory env in
                   let v = Heap.has_field heap l field in
                   let locals = Store.add_exn locals x v in
                   State.Continue { c with locals; env } :: accum
               | Some cond' ->
                   let pc' =
                     ESet.add symb_env.path_condition
                       (Translator.translate cond')
                   in
                   if not (Batch.check symb_env.solver (ESet.to_list pc')) then
                     accum
                   else
                     let env = Env.clone env in
                     let heap = Env.get_memory env in
                     let v = Heap.has_field heap l field in
                     let locals = Store.add_exn locals x v in
                     let symb_env = { symb_env with path_condition = pc' } in
                     State.Continue { c with locals; env; symb_env } :: accum))
    | Stmt.AssignObjToList (x, e) ->
        let f env l pc' =
          let v =
            let h = Env.get_memory env in
            match Heap.get h l with
            | None -> Crash.error stmt.at ("'" ^ l ^ "' not found in heap")
            | Some obj ->
                Expr.NOpt
                  ( Operators.ListExpr,
                    List.map (Object.to_list obj) ~f:(fun (f, v) ->
                        Expr.NOpt (Operators.TupleExpr, [ f; v ])) )
          in
          let locals = Store.add_exn locals x v in
          let symb_env = { symb_env with path_condition = pc' } in
          State.Continue { c with locals; env; symb_env }
        in
        let* e' = eval_reduce_expr locals e in
        let* locs = Expr.loc e' in
        return
          (List.fold locs ~init:[] ~f:(fun accum (cond, l) ->
               match cond with
               | None ->
                   let env' =
                     if List.length locs > 1 then Env.clone env else env
                   in
                   f env' l symb_env.path_condition :: accum
               | Some cond' ->
                   let pc' =
                     ESet.add symb_env.path_condition
                       (Translator.translate cond')
                   in
                   if not (Batch.check symb_env.solver (ESet.to_list pc')) then
                     accum
                   else f (Env.clone env) l pc' :: accum))
    | Stmt.AssignObjFields (x, e) ->
        let f env l pc' =
          let v =
            let h = Env.get_memory env in
            match Heap.get h l with
            | None -> Crash.error stmt.at ("'" ^ l ^ "' not found in heap")
            | Some obj -> Expr.NOpt (Operators.ListExpr, Object.get_fields obj)
          in
          let locals = Store.add_exn locals x v in
          let symb_env = { symb_env with path_condition = pc' } in
          State.Continue { c with locals; env; symb_env }
        in
        let* e' = eval_reduce_expr locals e in
        let* locs = Expr.loc e' in
        return
          (List.fold locs ~init:[] ~f:(fun accum (cond, l) ->
               match cond with
               | None ->
                   let env' =
                     if List.length locs > 1 then Env.clone env else env
                   in
                   f env' l symb_env.path_condition :: accum
               | Some cond' ->
                   let pc' =
                     ESet.add symb_env.path_condition
                       (Translator.translate cond')
                   in
                   if not (Batch.check symb_env.solver (ESet.to_list pc')) then
                     accum
                   else f (Env.clone env) l pc' :: accum))
    | Stmt.FieldAssign (e_loc, e_field, e_v) ->
        let* e_loc' = eval_reduce_expr locals e_loc in
        let* locs = Expr.loc e_loc' in
        let* reduced_field = eval_reduce_expr locals e_field in
        let* v = eval_reduce_expr locals e_v in
        return
          (List.fold locs ~init:[] ~f:(fun accum (cond, l) ->
               match cond with
               | None ->
                   let heap = Env.get_memory env in
                   let heap' =
                     if List.length locs > 1 then Heap.clone heap else heap
                   in
                   let pc = symb_env.path_condition in
                   let objects =
                     Heap.set_field heap' l reduced_field v symb_env.solver
                       (ESet.to_list pc)
                   in
                   List.map objects ~f:(fun (new_heap, new_pc) ->
                       let pc' = List.fold new_pc ~init:pc ~f:ESet.add in
                       let env = Env.add_memory env new_heap in
                       let symb_env = { symb_env with path_condition = pc' } in
                       State.Continue { c with env; symb_env })
               | Some cond' ->
                   let pc = symb_env.path_condition in
                   let pc' = ESet.add pc (Translator.translate cond') in
                   if not (Batch.check symb_env.solver (ESet.to_list pc')) then
                     accum
                   else
                     let heap = Env.get_memory env in
                     let objects =
                       Heap.set_field heap l reduced_field v symb_env.solver
                         (ESet.to_list pc)
                     in
                     List.map objects ~f:(fun (new_heap, new_pc) ->
                         let pc' = List.fold new_pc ~init:pc ~f:ESet.add in
                         let env = Env.add_memory env new_heap in
                         let symb_env =
                           { symb_env with path_condition = pc' }
                         in
                         State.Continue { c with env; symb_env })))
    | Stmt.FieldDelete (e_loc, e_field) ->
        let* e_loc' = eval_reduce_expr locals e_loc in
        let* locs = Expr.loc e_loc' in
        let* reduced_field = eval_reduce_expr locals e_field in
        return
          (List.fold locs ~init:[] ~f:(fun accum (cond, l) ->
               match cond with
               | None ->
                   let heap = Env.get_memory env in
                   let heap' =
                     if List.length locs > 1 then Heap.clone heap else heap
                   in
                   let pc = symb_env.path_condition in
                   let objects =
                     Heap.delete_field heap' l reduced_field symb_env.solver
                       (ESet.to_list pc)
                   in
                   List.map objects ~f:(fun (new_heap, new_pc) ->
                       let pc' = List.fold new_pc ~init:pc ~f:ESet.add in
                       let env = Env.add_memory env new_heap in
                       let symb_env = { symb_env with path_condition = pc' } in
                       State.Continue { c with env; symb_env })
               | Some cond' ->
                   let pc = symb_env.path_condition in
                   let pc' = ESet.add pc (Translator.translate cond') in
                   if not (Batch.check symb_env.solver (ESet.to_list pc')) then
                     accum
                   else
                     let heap = Env.get_memory env in
                     let objects =
                       Heap.delete_field heap l reduced_field symb_env.solver
                         (ESet.to_list pc)
                     in
                     List.map objects ~f:(fun (new_heap, new_pc) ->
                         let pc' = List.fold new_pc ~init:pc ~f:ESet.add in
                         let env = Env.add_memory env new_heap in
                         let symb_env =
                           { symb_env with path_condition = pc' }
                         in
                         State.Continue { c with env; symb_env })))
    | Stmt.FieldLookup (x, e_loc, e_field) ->
        let* e_loc' = eval_reduce_expr locals e_loc in
        let* locs = Expr.loc e_loc' in
        let* reduced_field = eval_reduce_expr locals e_field in
        return
          (List.fold locs ~init:[] ~f:(fun accum (cond, l) ->
               match cond with
               | None ->
                   let heap = Env.get_memory env in
                   let heap' =
                     if List.length locs > 1 then Heap.clone heap else heap
                   in
                   let pc = symb_env.path_condition in
                   let objects =
                     Heap.get_field heap' l reduced_field symb_env.solver
                       (ESet.to_list pc)
                   in
                   List.map objects ~f:(fun (new_heap, new_pc, v) ->
                       let v' =
                         Option.value v
                           ~default:(Expr.Val (Val.Symbol "undefined"))
                       in
                       let pc' = List.fold new_pc ~init:pc ~f:ESet.add in
                       let locals = Store.add_exn locals x v' in
                       let env = Env.add_memory env new_heap in
                       let symb_env = { symb_env with path_condition = pc' } in
                       State.Continue { c with locals; env; symb_env })
               | Some cond' ->
                   let pc = symb_env.path_condition in
                   let pc' = ESet.add pc (Translator.translate cond') in
                   if not (Batch.check symb_env.solver (ESet.to_list pc')) then
                     accum
                   else
                     let heap = Env.get_memory env in
                     let objects =
                       Heap.get_field heap l reduced_field symb_env.solver
                         (ESet.to_list pc)
                     in
                     List.map objects ~f:(fun (new_heap, new_pc, v) ->
                         let v' =
                           Option.value v
                             ~default:(Expr.Val (Val.Symbol "undefined"))
                         in
                         let pc' = List.fold new_pc ~init:pc ~f:ESet.add in
                         let locals = Store.add_exn locals x v' in
                         let env = Env.add_memory env new_heap in
                         let symb_env =
                           { symb_env with path_condition = pc' }
                         in
                         State.Continue { c with locals; env; symb_env })))
    | Stmt.SymStmt (SymStmt.Assume e) -> (
        let* e' = eval_reduce_expr locals e in
        (* TODO: Do not discharge to solver (saves 1 query per assume) *)
        match e' with
        | Expr.Val (Val.Bool b) -> if b then st locals else return []
        | e' ->
            let pc' =
              ESet.add symb_env.path_condition (Translator.translate e')
            in
            let symb_env = { symb_env with path_condition = pc' } in
            return [ State.Continue { c with symb_env } ])
    | Stmt.SymStmt (SymStmt.Evaluate (_x, e)) ->
        let* e' = eval_reduce_expr locals e in
        let e' = Translator.translate e' in
        let _sym_e = List.hd (Encoding.Expression.get_symbols [ e' ]) in
        assert (
          Batch.check symb_env.solver (ESet.to_list symb_env.path_condition));
        assert false
    | Stmt.SymStmt (SymStmt.Maximize (x, e)) ->
        let* e' = eval_reduce_expr locals e in
        let e' = Translator.translate e' in
        let pc = ESet.to_list symb_env.path_condition in
        let v =
          Option.map ~f:Translator.expr_of_value
            (Encoding.Optimizer.maximize symb_env.optimizer e' pc)
        in
        st
        @@ Store.add_exn locals x (Option.value ~default:(Expr.Val Val.Null) v)
    | Stmt.SymStmt (SymStmt.Minimize (x, e)) ->
        let* e' = eval_reduce_expr locals e in
        let e' = Translator.translate e' in
        let pc = ESet.to_list symb_env.path_condition in
        let v =
          Option.map ~f:Translator.expr_of_value
            (Encoding.Optimizer.minimize symb_env.optimizer e' pc)
        in
        st
        @@ Store.add_exn locals x (Option.value ~default:(Expr.Val Val.Null) v)
    | Stmt.SymStmt (SymStmt.Is_symbolic (x, e)) ->
        let* e' = eval_reduce_expr locals e in
        st @@ Store.add_exn locals x (Expr.Bool.const (Expr.is_symbolic e'))
    | Stmt.SymStmt (SymStmt.Is_sat (x, e)) ->
        let* e' = eval_reduce_expr locals e in
        let pc' = ESet.add symb_env.path_condition (Translator.translate e') in
        let sat = Batch.check symb_env.solver (ESet.to_list pc') in
        st @@ Store.add_exn locals x (Expr.Bool.const sat)
    | Stmt.SymStmt (SymStmt.Is_number (x, e)) ->
        let* e' = eval_reduce_expr locals e in
        let is_num =
          match Sval_typing.type_of e' with
          | Some Type.IntType | Some Type.FltType -> true
          | _ -> false
        in
        st @@ Store.add_exn locals x (Expr.Bool.const is_num)

  module type WorkList = sig
    type 'a t

    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
    val length : 'a t -> int
  end

  (* Source: Thanks to Joao Borges (@RageKnify) for writing this code *)
  module TreeSearch (L : WorkList) = struct
    open State

    let eval c : State.exec_state list =
      let w = L.create () in
      L.push c w;
      let out = ref [] in
      while not (L.is_empty w) do
        let c = L.pop w in
        match c.stmts with
        | stmt :: stmts -> (
            let states = exec_stmt stmt { c with stmts } in
            match states with
            | Ok states ->
                List.iter states ~f:(fun state ->
                    match state with
                    | State.Continue c -> L.push c w
                    | State.Return _ -> out := c :: !out)
            | Error msg ->
                Log.debug (lazy (sprintf "error    : %s: %s" c.func msg)))
        | [] ->
            Format.printf "Empty continuation!@.";
            assert false
      done;
      !out
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

  let invoke (env : Env.t) (func : Func.t)
      (eval : State.exec_state -> State.exec_state list) : State.exec_state list
      =
    let state = State.empty_state ~env ~func:func.name in
    eval State.{ state with stmts = [ func.body ] }

  let analyse (env : Env.t) (f : string) (policy : string) :
      State.exec_state list =
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
    invoke env f eval

  let main (env : Env.t) (f : string) : unit =
    let open State in
    let time_analysis = ref 0.0 in
    let configs =
      Time_utils.time_call time_analysis (fun () ->
          analyse env f !Config.policy)
    in
    let testsuite_path = Filename.concat !Config.workspace "test-suite" in
    Io.safe_mkdir testsuite_path;
    let final_configs = configs in
    let error_configs = [] in
    let f c =
      let pc' = State.ESet.to_list c.symb_env.path_condition in
      assert (Batch.check c.symb_env.solver pc');
      let model = Batch.model c.symb_env.solver in
      let open Encoding in
      let testcase =
        Option.value_map model ~default:[] ~f:(fun m ->
            List.map (Model.get_bindings m) ~f:(fun (s, v) ->
                let sort = Types.string_of_type (Symbol.type_of s)
                and name = Symbol.to_string s
                and interp = Value.to_string v in
                (sort, name, interp)))
      in
      let pc = (Expression.string_of_pc pc', Expression.to_smt pc') in
      let sink = ("", "") in
      (sink, pc, testcase)
    in
    let serialize cs prefix =
      let cs' = List.map ~f cs in
      let prefix' = Filename.concat testsuite_path prefix in
      let sinks = List.map ~f:(fun (sink, _, _) -> sink) cs' in
      let queries = List.map ~f:(fun (_, pc, _) -> pc) cs' in
      let testsuite = List.map ~f:(fun (_, _, testcase) -> testcase) cs' in
      Report.serialise_sinks sinks prefix';
      Report.serialise_queries queries prefix';
      Report.serialise_testsuite testsuite prefix'
    in
    serialize final_configs "testcase";
    serialize error_configs "witness";
    Report.serialise_report
      (Filename.concat !Config.workspace "report.json")
      !Config.file (List.length configs)
      (List.length error_configs)
      0 !time_analysis 0.0
end
