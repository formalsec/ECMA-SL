open Source
open Stmt

let ( !! ) res =
  match res with
  | Ok v -> v
  | Error err -> Eslerr.(runtime (RuntimeErr.Custom err))

module M (Mon : Monitor.M) = struct
  type obj = Val.t Object.t
  type store = Val.t Store.t
  type heap = Val.t Heap.t
  type stack = store Call_stack.t
  type state = stack * store * heap * string

  type return =
    | Final of Val.t
    | Error of Val.t
    | Intermediate of state * Stmt.t list

  let initial_state (main : string) : state =
    let stack = Call_stack.push Call_stack.empty Call_stack.Toplevel in
    let store = Store.create [] in
    let heap = Heap.create () in
    (stack, store, heap, main)

  let val_to_string (heap : heap) (v : Val.t) : string =
    match v with
    | Str s -> s
    | Loc l -> !!(Heap.get heap l) |> Object.str (Val.str ~flt_with_dot:false)
    | _ -> Val.str v

  let eval_operator_safe (eval_op_fun : unit -> Val.t) (es : Expr.t list) :
    Val.t =
    try eval_op_fun ()
    with Eslerr.Runtime_error _ as exn ->
      let e = Eslerr.(src exn |> index_to_el es) in
      Eslerr.(set_src (Expr e) exn |> raise)

  let rec eval_expr (store : store) (expr : Expr.t) : Val.t =
    match expr with
    | Val v -> v
    | Var x -> !!(Store.get store x)
    | UnOpt (op, e) ->
      let v = eval_expr store e in
      let eval_op_fun () = Eval_operator.eval_unopt op v in
      eval_operator_safe eval_op_fun [ e ]
    | BinOpt (op, e1, e2) ->
      let v1 = eval_expr store e1 in
      let v2 = eval_expr store e2 in
      let eval_op_fun () = Eval_operator.eval_binopt op v1 v2 in
      eval_operator_safe eval_op_fun [ e1; e2 ]
    | TriOpt (op, e1, e2, e3) ->
      let v1 = eval_expr store e1 in
      let v2 = eval_expr store e2 in
      let v3 = eval_expr store e3 in
      let eval_op_fun () = Eval_operator.eval_triopt op v1 v2 v3 in
      eval_operator_safe eval_op_fun [ e1; e2; e3 ]
    | NOpt (op, es) ->
      let vs = List.map (eval_expr store) es in
      let eval_op_fun () = Eval_operator.eval_nopt op vs in
      eval_operator_safe eval_op_fun es
    | Curry (f, es) -> (
      let fv = eval_expr store f in
      let vs = List.map (eval_expr store) es in
      match fv with
      | Str s -> Val.Curry (s, vs)
      | _ -> Eslerr.(runtime ~src:(Expr f) (RuntimeErr.BadExpr ("curry", fv))) )
    | Symbolic (t, _) -> (
      Random.self_init ();
      match t with
      | Type.IntType -> Val.Int (Random.int 128)
      | Type.FltType -> Val.Flt (Random.float 128.0)
      | _ -> Eslerr.internal __FUNCTION__ (NotImplemented (Some "symbolic")) )

  let eval_string (store : store) (expr : Expr.t) : string =
    match eval_expr store expr with
    | Str s -> s
    | _ as v ->
      Eslerr.(runtime ~src:(Expr expr) (RuntimeErr.BadVal ("string", v)))

  let eval_boolean (store : store) (expr : Expr.t) : bool =
    match eval_expr store expr with
    | Bool b -> b
    | _ as v ->
      Eslerr.(runtime ~src:(Expr expr) (RuntimeErr.BadVal ("boolean", v)))

  let eval_location (store : store) (expr : Expr.t) : string =
    match eval_expr store expr with
    | Loc l -> l
    | _ as v ->
      Eslerr.(runtime ~src:(Expr expr) (RuntimeErr.BadVal ("location", v)))

  let eval_object (store : store) (heap : heap) (expr : Expr.t) : string * obj =
    let loc = eval_location store expr in
    let obj = !!(Heap.get heap loc) in
    (loc, obj)

  let get_func_id (store : store) (fexpr : Expr.t) : string * Val.t list =
    match eval_expr store fexpr with
    | Val.Str fn -> (fn, [])
    | Val.Curry (fn, fvs) -> (fn, fvs)
    | _ as v -> Eslerr.(runtime ~src:(Expr fexpr) (RuntimeErr.BadFuncId v))

  let prepare_call (stack : stack) (store : store) (cont : Stmt.t list)
    (x : string) (func : Func.t) (vs : Val.t list) : stack * store =
    let fn = Func.name func in
    let new_frame = Call_stack.Intermediate (cont, store, x, fn) in
    let stack' = Call_stack.push stack new_frame in
    let params = Func.params func in
    let store' =
      try List.combine params vs |> Store.create
      with _ ->
        let (nparams, nargs) = (List.length params, List.length vs) in
        Eslerr.(runtime (RuntimeErr.BadNArgs (nparams, nargs)))
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
        let func = !!(Prog.func prog fn) in
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
      | (true, _, _) -> Eslerr.internal __FUNCTION__ (Expecting "if block")
      | (false, _, _) -> Eslerr.internal __FUNCTION__ (Expecting "else block") )
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

  let eval_small_step_safe (prog : Prog.t) (state : state) (stmt : Stmt.t)
    (cont : Stmt.t list) : return * Mon.sl_label =
    try eval_small_step prog state stmt cont
    with Eslerr.Runtime_error _ as exn ->
      Eslerr.(set_loc (Stmt stmt) exn |> raise)

  let rec small_step_iter (prog : Prog.t) (state : state)
    (mon_state : Mon.state) (stmts : Stmt.t list) : return =
    match stmts with
    | [] -> Eslerr.internal __FUNCTION__ (Expecting "non-empty stmt list")
    | s :: stmts' -> (
      let (return, label) = eval_small_step_safe prog state s stmts' in
      let mon_return = Mon.eval_small_step mon_state label in
      let mon_state' = Mon.next_state mon_return in
      match return with
      | Final v -> Final v
      | Error v -> Error v
      | Intermediate (state', stmts'') ->
        small_step_iter prog state' mon_state' stmts'' )

  let eval_prog ?(main : string = "main") (prog : Prog.t) : return =
    let func = !!(Prog.func prog main) in
    let state = initial_state main in
    let mon_state = Mon.initial_state () in
    let return = small_step_iter prog state mon_state [ func.body ] in
    match return with
    | Final _ as retval -> retval
    | Error err as retval ->
      Printf.printf "uncaught exception: %s" (Val.str err);
      retval
    | _ -> Eslerr.internal __FUNCTION__ (Expecting "final/error return")
end
