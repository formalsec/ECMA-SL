open Core
open Source

let ( let* ) o f = match o with Error e -> failwith e | Ok o -> f o
let ( let+ ) o f = Result.map o ~f

let list_map ~f l =
  let exception E of string in
  try
    Ok
      (List.map l ~f:(fun v ->
         match f v with Error s -> raise (E s) | Ok v -> v ) )
  with E s -> Error s

module Crash = Err.Make ()
module Invalid_arg = Err.Make ()

exception Crash = Crash.Error
exception Invalid_arg = Invalid_arg.Error

module Make (P : Eval_functor_intf.P) :
  Eval_functor_intf.S
    with type env := P.env
     and type 'a choice := 'a P.Choice.t
     and type value = P.value = struct
  module Value = P.Value
  module Extern_func = P.Extern_func
  module Store = P.Store
  module Object = P.Object
  module Heap = P.Heap
  module Env = P.Env
  module Choice = P.Choice
  module Reducer = P.Reducer

  type value = P.value
  type store = P.store

  let ( let/ ) = Choice.bind

  (* let ( let*/ ) o f = *)
  (*   match o with Error e -> failwith e | Ok o -> Choice.bind o f *)

  module State = struct
    type store = Store.t
    type nonrec env = P.env

    type exec_state =
      { return_state : (exec_state * string) option
      ; locals : store
      ; stmts : Stmt.t list
      ; env : env (* ; func : string *)
      }

    let empty_state ~env =
      { return_state = None
      ; locals = Store.create []
      ; stmts = []
      ; env (* ; func = "" *)
      }

    type stmt_result =
      | Return of exec_state
      | Continue of exec_state

    type _stmt_err =
      | Error of string
      | Assertion of value list
      | Unknown of value list

    let return (state : exec_state) (v : value) : stmt_result =
      match state.return_state with
      | None -> Return state
      | Some (state', ret_v) ->
        let locals = Store.add_exn state'.locals ret_v v in
        let env = state.env in
        Continue { state' with locals; env }
  end

  let eval_reduce_expr (sto : store) (e : Expr.t) : (value, string) Result.t =
    let+ e' = Value.eval_expr sto e in
    Reducer.reduce e'

  let pp locals e =
    (* TODO: Print function in sym_value *)
    (* let s = *)
    (*   match e' with *)
    (*   | Expr.Val (Val.Loc l) -> *)
    (*     let heap = Env.get_memory env in *)
    (*     let o = Heap.get heap l in *)
    (*     Object.to_string (Option.value_exn o) Expr.str *)
    (*   | _ -> Expr.str e' *)
    (* in *)
    (* Printf.printf "print:%s\npc:%s\nheap id:%d\n" s (Encoding.Expression.string_of_pc pc) (Heap.get_id heap); *)
    let* v = eval_reduce_expr locals e in
    Value.Pp.pp v

  let exec_func state func args ret_var =
    Log.debug (lazy (sprintf "calling func: %s" (Func.get_name func)));
    let return_state = Some (state, ret_var) in
    let params = Func.get_params func in
    let store = Store.create (List.zip_exn params args) in
    let state' =
      State.
        { return_state
        ; locals = store
        ; stmts = [ Func.get_body func ]
        ; env = state.env (* ; func = func.name *)
        }
    in
    Choice.return @@ State.Continue state'

  let exec_extern_func state f args ret_var =
    let open Extern_func in
    let rec apply :
      type a. value Stack.t -> a Extern_func.atype -> a -> value Choice.t =
     fun args ty f ->
      match ty with
      | UArg ty' -> apply args ty' (f ())
      | Arg ty' ->
        let v = Stack.pop_exn args in
        apply args ty' (f v)
      | Res -> f
    in
    let (Extern_func (Func atype, func)) = f in
    let/ v = apply (Stack.of_list args) atype func in
    let locals = Store.add_exn state.State.locals ret_var v in
    Choice.return @@ State.Continue State.{ state with locals }

  let exec_stmt stmt (state : State.exec_state) : State.stmt_result Choice.t =
    let open State in
    let { locals; env; _ } = state in
    let st locals = Choice.return @@ State.Continue { state with locals } in
    Log.debug
      (lazy (sprintf "store       : %s" (Value.Pp.Store.to_string locals)));
    Log.debug
      (lazy (sprintf "running stmt: %s" (Stmt.Pp.to_string stmt (pp locals))));
    match stmt.it with
    | Stmt.Skip -> st locals
    | Stmt.Merge -> st locals
    | Stmt.Exception err ->
      let at' = Source.string_of_region stmt.at in
      Choice.error (sprintf "%s: Exception: %s" at' err)
    | Stmt.Fail e -> Choice.error (sprintf "fail: %s" (pp locals e))
    | Stmt.Abort e -> Choice.error (sprintf "abort: %s" (pp locals e))
    | Stmt.Print e ->
      Format.printf "%s@." (pp locals e);
      st locals
    | Stmt.Assign (x, e) ->
      let* v = eval_reduce_expr locals e in
      st @@ Store.add_exn locals x v
    | Stmt.Assert e ->
      let* e' = eval_reduce_expr locals e in
      let/ b = Choice.select @@ Value.Bool.not_ e' in
      if b then Choice.error (sprintf "assert: %s" (Value.Pp.pp e'))
      else st locals
    | Stmt.Block blk ->
      Choice.return @@ State.Continue { state with stmts = blk @ state.stmts }
    | Stmt.If (br, blk1, blk2) ->
      let* br = eval_reduce_expr locals br in
      let/ b = Choice.branch br in
      let stmts =
        if b then blk1 :: state.stmts
        else Option.fold blk2 ~init:state.stmts ~f:(fun a b -> b :: a)
      in
      Choice.return @@ State.Continue { state with stmts }
    | Stmt.While (br, blk) ->
      let blk' =
        Stmt.Block (blk :: [ Stmt.While (br, blk) @> stmt.at ]) @> blk.at
      in
      let stmts = (Stmt.If (br, blk', None) @> stmt.at) :: state.stmts in
      Choice.return @@ State.Continue { state with stmts }
    | Stmt.Return e ->
      let* v = eval_reduce_expr locals e in
      Choice.return @@ State.return state v
    | Stmt.AssignCall (x, f, es) ->
      let* f' = eval_reduce_expr locals f in
      let* func_name, args0 = Value.get_func_name f' in
      let* func = Env.get_func env func_name in
      let* args = list_map ~f:(eval_reduce_expr locals) es in
      let args = args0 @ args in
      exec_func state func args x
    | Stmt.AssignECall (x, f, es) ->
      let* func = Env.get_extern_func env f in
      let* args = list_map ~f:(eval_reduce_expr locals) es in
      exec_extern_func state func args x
    | Stmt.AssignNewObj x ->
      let/ heap = Env.get_memory env in
      let obj = Object.create () in
      let loc = Heap.insert heap obj in
      st @@ Store.add_exn locals x loc
    | Stmt.AssignInObjCheck (x, e_field, e_loc) ->
      let* field = eval_reduce_expr locals e_field in
      let* loc = eval_reduce_expr locals e_loc in
      let/ loc = Heap.loc loc in
      let/ heap = Env.get_memory env in
      let v = Heap.has_field heap loc field in
      st @@ Store.add_exn locals x v
    | Stmt.AssignObjToList (x, e) -> (
      let* loc = eval_reduce_expr locals e in
      let/ loc = Heap.loc loc in
      let/ heap = Env.get_memory env in
      match Heap.get heap loc with
      | None -> Choice.error (sprintf "'%s' not found in heap" loc)
      | Some o ->
        let v = Value.mk_list (List.map (Object.to_list o) ~f:Value.mk_tuple) in
        st @@ Store.add_exn locals x v )
    | Stmt.AssignObjFields (x, e) -> (
      let* loc = eval_reduce_expr locals e in
      let/ loc = Heap.loc loc in
      let/ heap = Env.get_memory env in
      match Heap.get heap loc with
      | None -> Choice.error (sprintf "'%s' not found in heap" loc)
      | Some o ->
        let v = Value.mk_list @@ Object.get_fields o in
        st @@ Store.add_exn locals x v )
    | Stmt.FieldAssign (e_loc, e_field, e_v) ->
      let* loc = eval_reduce_expr locals e_loc in
      let* field = eval_reduce_expr locals e_field in
      let* v = eval_reduce_expr locals e_v in
      let/ loc = Heap.loc loc in
      let/ heap = Env.get_memory env in
      Heap.set_field heap loc ~field ~data:v;
      st locals
    | Stmt.FieldDelete (e_loc, e_field) ->
      let* loc = eval_reduce_expr locals e_loc in
      let* field = eval_reduce_expr locals e_field in
      let/ loc = Heap.loc loc in
      let/ heap = Env.get_memory env in
      Heap.delete_field heap loc field;
      st locals
    | Stmt.FieldLookup (x, e_loc, e_field) ->
      let* loc = eval_reduce_expr locals e_loc in
      let* field = eval_reduce_expr locals e_field in
      let/ loc = Heap.loc loc in
      let/ heap = Env.get_memory env in
      let/ value = Heap.get_field heap loc field in
      let value' = Option.value value ~default:(Value.mk_symbol "undefined") in
      st @@ Store.add_exn locals x value'

  (* | Stmt.SymStmt (SymStmt.Evaluate (_x, _e)) -> *)
  (*   assert false *)
  (*   let* e' = eval_reduce_expr locals e in *)
  (*   let e' = Translator.translate e' in *)
  (*   let _sym_e = List.hd (Encoding.Expression.get_symbols [ e' ]) in *)
  (*   assert ( *)
  (*     Batch.check symb_env.solver (ESet.to_list symb_env.path_condition)); *)
  (*   assert false *)
  (* | Stmt.SymStmt (SymStmt.Maximize (_x, _e)) -> *)
  (*   assert false *)
  (*   let* e' = eval_reduce_expr locals e in *)
  (*   let e' = Translator.translate e' in *)
  (*   let pc = ESet.to_list symb_env.path_condition in *)
  (*   let v = *)
  (*     Option.map ~f:Translator.expr_of_value *)
  (*       (Encoding.Optimizer.maximize symb_env.optimizer e' pc) *)
  (*   in *)
  (*   st *)
  (*   @@ Store.add_exn locals x (Option.value ~default:(Expr.Val Val.Null) v) *)
  (* | Stmt.SymStmt (SymStmt.Minimize (_x, _e)) -> *)
  (*   assert false *)
  (*   let* e' = eval_reduce_expr locals e in *)
  (*   let e' = Translator.translate e' in *)
  (*   let pc = ESet.to_list symb_env.path_condition in *)
  (*   let v = *)
  (*     Option.map ~f:Translator.expr_of_value *)
  (*       (Encoding.Optimizer.minimize symb_env.optimizer e' pc) *)
  (*   in *)
  (*   st *)
  (*   @@ Store.add_exn locals x (Option.value ~default:(Expr.Val Val.Null) v) *)

  (* let serialize = *)
  (*   let open State in *)
  (*   let counter = ref 0 in *)
  (*   fun ?(witness : string option) (state : State.exec_state) -> *)
  (*     let pc = State.ESet.to_list state.symb_env.pc in *)
  (*     assert (Batch.check state.symb_env.solver pc); *)
  (*     let model = Batch.model state.symb_env.solver in *)
  (*     let testcase = *)
  (*       Option.value_map model ~default:"[]" ~f:(fun m -> *)
  (*         let open Encoding in *)
  (*         let inputs = *)
  (*           List.map (Model.get_bindings m) ~f:(fun (s, v) -> *)
  (*             let sort = Types.string_of_type (Symbol.type_of s) in *)
  (*             let name = Symbol.to_string s in *)
  (*             let interp = Value.to_string v in *)
  (*             sprintf *)
  (*               "{ \"type\" : \"%s\", \"name\" : \"%s\", \"value\" : \"%s\" }" *)
  (*               sort name interp ) *)
  (*         in *)
  (*         String.concat ~sep:", " inputs ) *)
  (*     in *)
  (*     let str_pc = Encoding.Expression.string_of_pc pc in *)
  (*     let smt_query = Encoding.Expression.to_smt pc in *)
  (*     let prefix = *)
  (*       incr counter; *)
  (*       let fname = if Option.is_some witness then "witness" else "testecase" in *)
  (*       let fname = sprintf "%s-%i" fname !counter in *)
  (*       Filename.concat (Filename.concat !Config.workspace "test-suite") fname *)
  (*     in *)
  (*     Io.write_file ~file:(sprintf "%s.json" prefix) ~data:testcase; *)
  (*     Io.write_file ~file:(sprintf "%s.pc" prefix) ~data:str_pc; *)
  (*     Io.write_file ~file:(sprintf "%s.smt2" prefix) ~data:smt_query; *)
  (*     Option.iter witness ~f:(fun sink -> *)
  (*       Io.write_file ~file:(sprintf "%s_sink.txt" prefix) ~data:sink ) *)

  (* Source: Thanks to Joao Borges (@RageKnify) for writing this code *)
  (* module type WorkList = sig *)
  (*   type 'a t *)

  (*   exception Empty *)

  (*   val create : unit -> 'a t *)
  (*   val push : 'a -> 'a t -> unit *)
  (*   val pop : 'a t -> 'a *)
  (*   val is_empty : 'a t -> bool *)
  (*   val length : 'a t -> int *)
  (* end *)

  (* Source: Thanks to Joao Borges (@RageKnify) for writing this code *)
  (* module RandArray : WorkList = struct *)
  (*   type 'a t = 'a BatDynArray.t *)

  (*   exception Empty *)

  (*   let create () = BatDynArray.create () *)
  (*   let is_empty a = BatDynArray.empty a *)
  (*   let push v a = BatDynArray.add a v *)

  (*   let pop a = *)
  (*     let i = Random.int (BatDynArray.length a) in *)
  (*     let v = BatDynArray.get a i in *)
  (*     BatDynArray.delete a i; *)
  (*     v *)

  (*   let length = BatDynArray.length *)
  (* end *)

  (* module TreeSearch (_ : WorkList) = struct *)
  (*   open State *)

  (*   let eval c = *)
  (*     let time = ref (Stdlib.Sys.time ()) in *)
  (*     let w = L.create () in *)
  (*     L.push c w; *)
  (*     while not (L.is_empty w) do *)
  (*       let c = L.pop w in *)
  (*       match c.stmts with *)
  (*       | stmt :: stmts -> ( *)
  (*         let states = exec_stmt stmt { c with stmts } in *)
  (*         match states with *)
  (*         | Ok results -> *)
  (*           List.iter results ~f:(fun result -> *)
  (*             match result with *)
  (*             | State.Continue state -> L.push state w *)
  (*             | State.Return state -> serialize state ) *)
  (*         | Error msg -> *)
  (*           serialize ~witness:msg c; *)
  (*           Format.printf "error       : %s: %s@." c.func msg ) *)
  (*       | [] -> *)
  (*         Format.printf "Empty continuation!@."; *)
  (*         assert false *)
  (*     done; *)
  (*     time := Stdlib.Sys.time () -. !time; *)
  (*     Format.printf "  exec time : %fs@." !time; *)
  (*     Format.printf "solver time : %fs@." !Batch.solver_time; *)
  (*     Format.printf "  mean time : %fms@." *)
  (*       (1000. *. !Batch.solver_time /. float !Batch.solver_count) *)
  (* end *)

  (* module DFS = TreeSearch (Stdlib.Stack) *)
  (* module BFS = TreeSearch (Stdlib.Queue) *)
  (* module RND = TreeSearch (RandArray) *)

  let rec loop (state : State.exec_state) : unit Choice.t =
    let open State in
    match state.stmts with
    | stmt :: stmts -> (
      let/ state = exec_stmt stmt { state with stmts } in
      match state with
      | State.Continue state -> loop state
      | State.Return _state -> Choice.return () )
    | [] ->
      Format.printf "Empty continuation!@.";
      assert false

  let main (env : Env.t) (f : string) : unit Choice.t =
    let testsuite_path = Filename.concat !Config.workspace "test-suite" in
    Io.safe_mkdir testsuite_path;
    (* let eval = *)
    (*   match policy with *)
    (*   | "breadth" -> BFS.eval *)
    (*   | "depth" -> DFS.eval *)
    (*   | "random" -> RND.eval *)
    (*   | _ -> *)
    (*     Crash.error f.body.at ("Invalid search policy '" ^ !Config.policy ^ "'") *)
    (* in *)
    let* f = Env.get_func env f in
    let state = State.empty_state ~env in
    loop State.{ state with stmts = [ Func.get_body f ] }
end
