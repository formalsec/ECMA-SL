open Source
open Syntax.Result

let ( let* ) o f = match o with Error e -> failwith e | Ok o -> f o

module Crash = Err.Make ()
module Invalid_arg = Err.Make ()

exception Crash = Crash.Error
exception Invalid_arg = Invalid_arg.Error

module Make (P : Interpreter_functor_intf.P) :
  Interpreter_functor_intf.S
    with type env := P.env
     and type 'a choice := 'a P.Choice.t
     and type value = P.value = struct
  module Value = P.Value
  module Extern_func = P.Extern_func
  module Store = P.Store
  module Object = P.Object
  module Memory = P.Memory
  module Env = P.Env
  module Choice = P.Choice
  module Reducer = P.Reducer

  type value = P.value
  type store = P.store

  let ( let/ ) = Choice.bind

  module State = struct
    type store = Store.t
    type nonrec env = P.env

    type exec_state =
      { return_state : (exec_state * string) option
      ; locals : store
      ; stmts : Stmt.t list
      ; env : env
      ; func : string
      }

    let empty_state ~env =
      { return_state = None
      ; locals = Store.create []
      ; stmts = []
      ; env
      ; func = ""
      }

    type return_result = (value list, string) Result.t

    type stmt_result =
      | Return of return_result
      | Continue of exec_state

    let return ?(value : value option) (state : exec_state) : stmt_result =
      match state.return_state with
      | None -> Return (Ok (Option.to_list value))
      | Some (state', ret_v) ->
        let v =
          Option.value
            ~default:Value.(mk_tuple (Bool.const false, mk_symbol "undefined"))
            value
        in
        let locals = Store.add_exn state'.locals ret_v v in
        Continue { state' with locals; env = state.env }
  end

  let eval_expr (sto : store) (e : Expr.t) : (value, string) Result.t =
    let+ e' = Value.eval_expr sto e in
    (* TODO: decouple Reducer from abstract values *)
    (* Reduce is only used on Sym_value.M.value *)
    Reducer.reduce e'

  let pp locals heap e =
    (* TODO: Print function in sym_value *)
    (* let s = *)
    (*   match e' with *)
    (*   | Expr.Val (Val.Loc l) -> *)
    (*     let heap = Env.get_memory env in *)
    (*     let o = Memory.get heap l in *)
    (*     Object.str (Option.value_exn o) Expr.str *)
    (*   | _ -> Expr.str e' *)
    (* in *)
    (* Printf.printf "print:%s\npc:%s\nheap id:%d\n" s (Encoding.Expression.string_of_pc pc) (Memory.get_id heap); *)
    let* v = eval_expr locals e in
    Memory.pp_val heap v

  let exec_func state func args ret_var =
    Log.debug "calling func: %s@." (Func.name func);
    let return_state = Some (state, ret_var) in
    let params = Func.params func in
    let store = Store.create (List.combine params args) in
    let state' =
      State.
        { return_state
        ; locals = store
        ; stmts = [ Func.body func ]
        ; env = state.env
        ; func = Func.name func
        }
    in
    Choice.return @@ State.Continue state'

  let exec_extern_func state f args ret_var =
    let open Extern_func in
    let rec apply :
      type a.
         value list
      -> a Extern_func.atype
      -> a
      -> (value, string) Result.t Choice.t =
     fun args ty f ->
      match ty with
      | UArg ty' -> apply args ty' (f ())
      | Arg ty' ->
        let v = List.hd args in
        apply (List.tl args) ty' (f v)
      | Res -> f
    in
    let (Extern_func (Func atype, func)) = f in
    let/ v = apply args atype func in
    match v with
    | Error _ as err -> Choice.return @@ State.Return err
    | Ok v ->
      let locals = Store.add_exn state.State.locals ret_var v in
      Choice.return @@ State.Continue State.{ state with locals }

  let exec_stmt stmt (state : State.exec_state) : State.stmt_result Choice.t =
    let open State in
    let { locals; env; _ } = state in
    let st locals = Choice.return @@ State.Continue { state with locals } in
    let err fmt =
      Format.kasprintf
        (fun msg -> Choice.return @@ State.Return (Error msg))
        fmt
    in
    let/ m = Env.get_memory env in
    Log.debug "      store : %s@." (Value.Pp.Store.to_string locals);
    Log.debug "running stmt: %a@." Stmt.pp stmt;
    match stmt.it with
    | Stmt.Skip -> st locals
    | Stmt.Merge -> st locals
    | Stmt.Debug s' ->
      Format.eprintf "ignoring break point in line %d" stmt.at.left.line;
      Choice.return @@ State.Continue { state with stmts = s' :: state.stmts }
    | Stmt.Fail e ->
      let e' = pp locals m e in
      Log.warn "       fail : %s@." e';
      err {|{ "fail" : "%S" }|} e'
    | Stmt.Print e ->
      Format.printf "%s@." (pp locals m e);
      st locals
    | Stmt.Assign (x, e) ->
      let* v = eval_expr locals e in
      st @@ Store.add_exn locals x v
    | Stmt.Assert e ->
      let* e' = eval_expr locals e in
      let/ b = Choice.check_add_true @@ Value.Bool.not_ e' in
      if b then (
        Log.warn "     assert : failure with (%a)@." Value.Pp.pp e';
        err {|{ "assert" : "%a" }|} Value.Pp.pp e' )
      else st locals
    | Stmt.Block blk ->
      Choice.return @@ State.Continue { state with stmts = blk @ state.stmts }
    | Stmt.If (br, blk1, blk2) ->
      let* br = eval_expr locals br in
      let/ b = Choice.branch br in
      let stmts =
        if b then blk1 :: state.stmts
        else
          Option.fold blk2 ~none:state.stmts ~some:(fun st -> st :: state.stmts)
      in
      Choice.return @@ State.Continue { state with stmts }
    | Stmt.While (br, blk) ->
      let blk' =
        Stmt.Block (blk :: [ Stmt.While (br, blk) @> stmt.at ]) @> blk.at
      in
      let stmts = (Stmt.If (br, blk', None) @> stmt.at) :: state.stmts in
      Choice.return @@ State.Continue { state with stmts }
    | Stmt.Return e ->
      let* v = eval_expr locals e in
      Choice.return @@ State.return state ~value:v
    | Stmt.AssignCall (x, f, es) ->
      let* f' = eval_expr locals f in
      let* (func_name, args0) = Value.get_func_name f' in
      let* func = Env.get_func env func_name in
      let* args = list_map ~f:(eval_expr locals) es in
      let args = args0 @ args in
      exec_func state func args x
    | Stmt.AssignECall (x, f, es) ->
      let* func = Env.get_extern_func env f in
      let* args = list_map ~f:(eval_expr locals) es in
      exec_extern_func state func args x
    | Stmt.AssignNewObj x ->
      let/ heap = Env.get_memory env in
      let obj = Object.create () in
      let loc = Memory.insert heap obj in
      st @@ Store.add_exn locals x loc
    | Stmt.AssignInObjCheck (x, e_field, e_loc) ->
      let* field = eval_expr locals e_field in
      let* loc = eval_expr locals e_loc in
      let/ loc = Memory.loc loc in
      (* `get_memory` comes after `Memory.loc` due to branching *)
      let/ heap = Env.get_memory env in
      let v = Memory.has_field heap loc field in
      st @@ Store.add_exn locals x v
    | Stmt.AssignObjToList (x, e) -> (
      let* loc = eval_expr locals e in
      let/ loc = Memory.loc loc in
      let/ heap = Env.get_memory env in
      match Memory.get heap loc with
      | None -> Choice.error (Format.sprintf "'%s' not found in heap" loc)
      | Some o ->
        let v = Value.mk_list (List.map Value.mk_tuple (Object.to_list o)) in
        st @@ Store.add_exn locals x v )
    | Stmt.AssignObjFields (x, e) -> (
      let* loc = eval_expr locals e in
      let/ loc = Memory.loc loc in
      let/ heap = Env.get_memory env in
      match Memory.get heap loc with
      | None -> Choice.error (Format.sprintf "'%s' not found in heap" loc)
      | Some o ->
        let v = Value.mk_list @@ Object.get_fields o in
        st @@ Store.add_exn locals x v )
    | Stmt.FieldAssign (e_loc, e_field, e_v) ->
      let* loc = eval_expr locals e_loc in
      let* field = eval_expr locals e_field in
      let* v = eval_expr locals e_v in
      let/ loc = Memory.loc loc in
      let/ heap = Env.get_memory env in
      Memory.set_field heap loc ~field ~data:v;
      st locals
    | Stmt.FieldDelete (e_loc, e_field) ->
      let* loc = eval_expr locals e_loc in
      let* field = eval_expr locals e_field in
      let/ loc = Memory.loc loc in
      let/ heap = Env.get_memory env in
      Memory.delete_field heap loc field;
      st locals
    | Stmt.FieldLookup (x, e_loc, e_field) ->
      let* loc = eval_expr locals e_loc in
      let* field = eval_expr locals e_field in
      let/ loc = Memory.loc loc in
      let/ heap = Env.get_memory env in
      let/ value = Memory.get_field heap loc field in
      let value' = Option.value value ~default:(Value.mk_symbol "undefined") in
      st @@ Store.add_exn locals x value'

  let rec loop (state : State.exec_state) : State.return_result Choice.t =
    let open State in
    match state.stmts with
    | stmt :: stmts -> (
      let/ state = exec_stmt stmt { state with stmts } in
      match state with
      | State.Continue state -> loop state
      | State.Return ret -> Choice.return ret )
    | [] -> (
      Format.printf "    warning : %s: missing a return statement!@." state.func;
      match State.return state with
      | State.Continue state -> loop state
      | State.Return ret -> Choice.return ret )

  let main (env : Env.t) (f : string) : State.return_result Choice.t =
    let* f = Env.get_func env f in
    let state = State.empty_state ~env in
    loop State.{ state with stmts = [ Func.body f ]; func = Func.name f }
end
