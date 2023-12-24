open Source
open Stmt

let ( let+ ) res f =
  match res with Ok v -> f v | Error err -> raise (Failure err)

let internal_err (err : string) : 'a = raise (Failure err)
let runtime_err (err : string) : 'a = raise (Failure err)

module M (Mon : Monitor.M) = struct
  type obj = Val.t Object.t
  type store = Val.t Store.t
  type heap = Val.t Heap.t
  type stack = store Call_stack.t
  type state = stack * store * heap * string

  type return =
    | Intermediate of state * Stmt.t list
    | Final of Val.t
    | Error of Val.t

  let initial_state (main : string) : state =
    let stack = Call_stack.push Call_stack.empty Call_stack.Toplevel in
    let store = Store.create [] in
    let heap = Heap.create () in
    (stack, store, heap, main)

  let val_to_string (heap : heap) (v : Val.t) : string =
    match v with
    | Loc l -> (
      match Heap.get heap l with
      | Some obj -> Object.str (Val.str ~flt_with_dot:false) obj
      | _ -> runtime_err (Printf.sprintf "Cannot find location '%s'" l) )
    | _ -> Val.str v

  let rec eval_expr (store : store) (expr : Expr.t) : Val.t =
    match expr with
    | Val v -> v
    | Var x -> (
      match Store.get store x with
      | Some v -> v
      | None -> runtime_err (Printf.sprintf "Cannot find variable '%s'" x) )
    | UnOpt (op, e) ->
      let v = eval_expr store e in
      Eval_operator.eval_unopt op v
    | BinOpt (op, e1, e2) ->
      let v1 = eval_expr store e1 in
      let v2 = eval_expr store e2 in
      Eval_operator.eval_binopt op v1 v2
    | TriOpt (op, e1, e2, e3) ->
      let v1 = eval_expr store e1 in
      let v2 = eval_expr store e2 in
      let v3 = eval_expr store e3 in
      Eval_operator.eval_triopt op v1 v2 v3
    | NOpt (op, es) ->
      let vs = List.map (eval_expr store) es in
      Eval_operator.eval_nopt op vs
    | Curry (f, es) -> (
      let fv = eval_expr store f in
      let vs = List.map (eval_expr store) es in
      match fv with
      | Str s -> Val.Curry (s, vs)
      | _ -> runtime_err "Illegal curry expression" )
    | Symbolic (t, _) -> (
      Random.self_init ();
      match t with
      | Type.IntType -> Val.Int (Random.int 128)
      | Type.FltType -> Val.Flt (Random.float 128.0)
      | _ ->
        internal_err
          (Printf.sprintf
             "Interpreter.eval_expr: symbolic \"%s\" not implemented"
             (Type.str t) ) )

  let eval_string (store : store) (expr : Expr.t) : string =
    match eval_expr store expr with
    | Str s -> s
    | _ ->
      runtime_err
        (Printf.sprintf "Expecting a string value, but got '%s'" (Expr.str expr))

  let eval_boolean (store : store) (expr : Expr.t) : bool =
    match eval_expr store expr with
    | Bool b -> b
    | _ ->
      runtime_err
        (Printf.sprintf "Expecting a boolean value, but got '%s'"
           (Expr.str expr) )

  let eval_location (store : store) (expr : Expr.t) : string =
    match eval_expr store expr with
    | Loc l -> l
    | _ ->
      runtime_err
        (Printf.sprintf "Expecting a location value, but got '%s'"
           (Expr.str expr) )

  let eval_object (store : store) (heap : heap) (expr : Expr.t) : string * obj =
    let loc = eval_location store expr in
    match Heap.get heap loc with
    | Some obj -> (loc, obj)
    | None -> runtime_err (Printf.sprintf "Location '%s' does not exits" loc)

  let get_func_id (store : store) (fexpr : Expr.t) : string * Val.t list =
    match eval_expr store fexpr with
    | Val.Str fn -> (fn, [])
    | Val.Curry (fn, fvs) -> (fn, fvs)
    | _ -> runtime_err "Wrong or invalid function id"

  let prepare_call (stack : stack) (store : store) (cont : Stmt.t list)
    (x : string) (func : Func.t) (vs : Val.t list) : stack * store =
    let fn = Func.name func in
    let new_frame = Call_stack.Intermediate (cont, store, x, fn) in
    let stack' = Call_stack.push stack new_frame in
    let params = Func.params func in
    let store' =
      try List.combine params vs |> Store.create
      with _ -> runtime_err ("Invalid number of arguments in " ^ fn)
    in
    (stack', store')

  let eval_small_step (prog : Prog.t) (state : state) (stmt : Stmt.t)
    (cont : Stmt.t list) : return * Mon.sl_label =
    let _label stmt_md = Mon.generate_label stmt stmt_md in
    let (stack, store, heap, _func) = state in
    match stmt.it with
    | Skip -> (Intermediate (state, cont), _label SkipLbl)
    | Merge -> (Intermediate (state, cont), _label MergeLbl)
    | Block block -> (Intermediate (state, block @ cont), _label BlockLbl)
    | Print e ->
      eval_expr store e |> val_to_string heap |> Printf.printf "%s";
      (Intermediate (state, cont), _label PrintLbl)
    | Return e -> (
      let v = eval_expr store e in
      let (frame, stack') = Call_stack.pop stack in
      match frame with
      | Call_stack.Toplevel -> (Final v, _label ReturnLbl)
      | Call_stack.Intermediate (cont', store', x, func') ->
        Store.set store' x v;
        let state' = (stack', store', heap, func') in
        (Intermediate (state', cont'), _label ReturnLbl) )
    | Assign (x, e) ->
      eval_expr store e |> Store.set store x;
      (Intermediate (state, cont), _label AssignLbl)
    | AssignCall (x, fexpr, es) -> (
      let (fn, fvs) = get_func_id store fexpr in
      let vs = fvs @ List.map (eval_expr store) es in
      match Mon.interceptor fn vs es with
      | Some label -> (Intermediate (state, cont), label)
      | None ->
        let+ func = Prog.func prog fn in
        let (stack', store') = prepare_call stack store cont x func vs in
        let state' = (stack', store', heap, fn) in
        let cont' = Func.body func :: [] in
        (Intermediate (state', cont'), _label (AssignCallLbl func)) )
    | AssignECall (x, fexpr, es) ->
      let vs = List.map (eval_expr store) es in
      let v = External.execute prog heap fexpr vs in
      Store.set store x v;
      (Intermediate (state, cont), _label AssignECallLbl)
    | AssignNewObj x ->
      let loc = Object.create () |> Heap.insert heap in
      Store.set store x (Val.Loc loc);
      (Intermediate (state, cont), _label (AssignNewObjLbl loc))
    | AssignObjToList (x, e) ->
      let _fld_to_tup_f (fn, fv) = Val.Tuple [ Str fn; fv ] in
      let (_, obj) = eval_object store heap e in
      let v = Val.List (Object.fld_list obj |> List.map _fld_to_tup_f) in
      Store.set store x v;
      (Intermediate (state, cont), _label AssignObjToListLbl)
    | AssignObjFields (x, e) ->
      let _fld_to_tup_f (fn, _) = Val.Str fn in
      let (_, obj) = eval_object store heap e in
      let v = Val.List (Object.fld_list obj |> List.map _fld_to_tup_f) in
      Store.set store x v;
      (Intermediate (state, cont), _label AssignObjFieldsLbl)
    | AssignInObjCheck (x, fe, oe) ->
      let _in_obj = function Some _ -> true | None -> false in
      let (loc, obj) = eval_object store heap oe in
      let fn = eval_string store fe in
      let v = Val.Bool (Object.get obj fn |> _in_obj) in
      Store.set store x v;
      (Intermediate (state, cont), _label (AssignInObjCheckLbl (loc, fn)))
    | FieldLookup (x, oe, fe) ->
      let _fld_val = function None -> Val.Symbol "undefined" | Some v -> v in
      let (loc, obj) = eval_object store heap oe in
      let fn = eval_string store fe in
      Object.get obj fn |> _fld_val |> Store.set store x;
      (Intermediate (state, cont), _label (FieldLookupLbl (loc, fn)))
    | FieldAssign (oe, fe, e) ->
      let (loc, obj) = eval_object store heap oe in
      let fn = eval_string store fe in
      let v = eval_expr store e in
      Object.set obj fn v;
      (Intermediate (state, cont), _label (FieldAssignLbl (loc, fn)))
    | FieldDelete (oe, fe) ->
      let (loc, obj) = eval_object store heap oe in
      let fn = eval_string store fe in
      Object.delete obj fn;
      (Intermediate (state, cont), _label (FieldDeleteLbl (loc, fn)))
    | If (e, s1, s2) -> (
      let v = eval_boolean store e in
      let s2' = Option.default (Skip @> no_region) s2 in
      match (v, s1.it, s2'.it) with
      | (true, Block block, _) ->
        let block' = block @ ((Stmt.Merge @> s1.at) :: cont) in
        (Intermediate (state, block'), _label (IfLbl true))
      | (false, _, Block block) ->
        let block' = block @ ((Stmt.Merge @> s2'.at) :: cont) in
        (Intermediate (state, block'), _label (IfLbl false))
      | (false, _, Skip) -> (Intermediate (state, cont), _label (IfLbl false))
      | (true, _, _) ->
        internal_err "Interpreter.eval_small_step: Expecting if block"
      | (false, _, _) ->
        internal_err "Interpreter.eval_small_step: Expecting else block" )
    | While (e, s) ->
      let loop_stmt = [ s; Stmt.While (e, s) @> s.at ] in
      let stmts = Stmt.If (e, Stmt.Block loop_stmt @> s.at, None) @> stmt.at in
      (Intermediate (state, stmts :: cont), _label WhileLbl)
    | Fail e ->
      let v = eval_expr store e in
      (Error v, _label FailLbl)
    | Assert e ->
      let v = eval_boolean store e in
      if v then (Intermediate (state, cont), _label (AssertLbl true))
      else
        let err = Printf.sprintf "Assert false: %s" (Expr.str e) in
        (Error (Val.Str err), _label (AssertLbl false))
    | Abort _ -> (Intermediate (state, cont), _label AbortLbl)

  let rec small_step_iter (prog : Prog.t) (state : state)
    (mon_state : Mon.state) (stmts : Stmt.t list) : return =
    match stmts with
    | [] -> internal_err "Interpreter.small_step_iter: empty stmt list"
    | s :: stmts' -> (
      let (return, label) = eval_small_step prog state s stmts' in
      let mon_return = Mon.eval_small_step mon_state label in
      let mon_state' = Mon.next_state mon_return in
      match return with
      | Final v -> Final v
      | Error v -> Error v
      | Intermediate (state', stmts'') ->
        small_step_iter prog state' mon_state' stmts'' )

  let eval_prog ?(main : string = "main") (prog : Prog.t) : Val.t =
    let+ func = Prog.func prog main in
    let state = initial_state main in
    let mon_state = Mon.initial_state () in
    let return = small_step_iter prog state mon_state [ func.body ] in
    match return with
    | Final v -> v
    | Error v -> runtime_err (Printf.sprintf "Failure: %s" (Val.str v))
    | _ -> internal_err "Interpreter.eval_prog: intermediate state at the end"
end
