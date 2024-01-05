open Source
open Stmt

module M (Mon : Monitor.M) = struct
  type value = Val.t
  type obj = value Object.t
  type store = value Store.t
  type heap = value Heap.t
  type stack = store Call_stack.t
  type state = store * heap * stack * Func.t

  type return =
    | Final of value
    | Error of value
    | Intermediate of state * Stmt.t list

  let eval_var (store : store) (x : string) : value =
    match Store.get_opt store x with
    | Some v -> v
    | None -> Eslerr.(runtime ~src:(Str x) (UnknownVar x))

  let eval_loc (heap : heap) (l : string) : obj =
    match Heap.get_opt heap l with
    | Some obj -> obj
    | None -> Eslerr.(runtime ~src:(Str l) (UnknownLoc l))

  let eval_func (prog : Prog.t) (fn : string) : Func.t =
    match Prog.func_opt prog fn with
    | Some func -> func
    | None -> Eslerr.(runtime ~src:(Str fn) (UnknownFunc fn))

  let eval_operator (eval_op_fun : unit -> value) (es : Expr.t list) : value =
    try eval_op_fun ()
    with Eslerr.Runtime_error _ as exn ->
      let e = Eslerr.(src exn |> index_to_el es) in
      Eslerr.(set_src (Expr e) exn |> raise)

  let initial_state (main : Func.t) : state =
    let stack = Call_stack.push Call_stack.empty (Call_stack.Toplevel main) in
    let store = Store.create [] in
    let heap = Heap.create () in
    (store, heap, stack, main)

  let print_val (heap : heap) (v : value) : unit =
    let open Fmt in
    match v with
    | Str s -> printf "%s@." s
    | Loc l -> printf "%a@." (Object.pp Val.pp) (eval_loc heap l)
    | _ -> printf "%a@." Val.pp v

  let rec eval_expr_sub (store : store) (e : Expr.t) : value =
    match e with
    | Val v -> v
    | Var x -> eval_var store x
    | UnOpt (op, e') ->
      let v = eval_expr store e' in
      let eval_op_fun () = Eval_operator.eval_unopt op v in
      eval_operator eval_op_fun [ e' ]
    | BinOpt (op, e1, e2) ->
      let v1 = eval_expr store e1 in
      let v2 = eval_expr store e2 in
      let eval_op_fun () = Eval_operator.eval_binopt op v1 v2 in
      eval_operator eval_op_fun [ e1; e2 ]
    | TriOpt (op, e1, e2, e3) ->
      let v1 = eval_expr store e1 in
      let v2 = eval_expr store e2 in
      let v3 = eval_expr store e3 in
      let eval_op_fun () = Eval_operator.eval_triopt op v1 v2 v3 in
      eval_operator eval_op_fun [ e1; e2; e3 ]
    | NOpt (op, es) ->
      let vs = List.map (eval_expr store) es in
      let eval_op_fun () = Eval_operator.eval_nopt op vs in
      eval_operator eval_op_fun es
    | Curry (fe, es) -> (
      let fv = eval_expr store fe in
      let vs = List.map (eval_expr store) es in
      match fv with
      | Str fn -> Val.Curry (fn, vs)
      | _ -> Eslerr.(runtime ~src:(Expr fe) (BadExpr ("curry", fv))) )
    | Symbolic (t, _) -> (
      Random.self_init ();
      match t with
      | Type.IntType -> Val.Int (Random.int 128)
      | Type.FltType -> Val.Flt (Random.float 128.0)
      | _ -> Eslerr.internal __FUNCTION__ (NotImplemented (Some "symbolic")) )

  and eval_expr (store : store) (e : Expr.t) : value =
    let v = eval_expr_sub store e in
    Verbose.eval_expr e v;
    v

  let eval_string (store : store) (e : Expr.t) : string =
    match eval_expr store e with
    | Str s -> s
    | _ as v -> Eslerr.(runtime ~src:(Expr e) (BadVal ("string", v)))

  let eval_boolean (store : store) (e : Expr.t) : bool =
    match eval_expr store e with
    | Bool b -> b
    | _ as v -> Eslerr.(runtime ~src:(Expr e) (BadVal ("boolean", v)))

  let eval_location (store : store) (e : Expr.t) : string =
    match eval_expr store e with
    | Loc l -> l
    | _ as v -> Eslerr.(runtime ~src:(Expr e) (BadVal ("location", v)))

  let eval_object (store : store) (heap : heap) (expr : Expr.t) : string * obj =
    let l = eval_location store expr in
    let obj = eval_loc heap l in
    (l, obj)

  let eval_func_expr (store : store) (fe : Expr.t) : string * value list =
    match eval_expr store fe with
    | Val.Str fn -> (fn, [])
    | Val.Curry (fn, fvs) -> (fn, fvs)
    | _ as v -> Eslerr.(runtime ~src:(Expr fe) (BadFuncId v))

  let prepare_call (stack : stack) (store : store) (cont : Stmt.t list)
    (x : string) (func : Func.t) (vs : value list) : stack * store =
    let params = Func.params func in
    let new_frame = Call_stack.Intermediate (cont, store, x, func) in
    let stack' = Call_stack.push stack new_frame in
    let store' =
      try List.combine params vs |> Store.create
      with _ ->
        let (nparams, nargs) = (List.length params, List.length vs) in
        Eslerr.(runtime (BadNArgs (nparams, nargs)))
    in
    (stack', store')

  let eval_small_step (prog : Prog.t) (state : state) (s : Stmt.t)
    (cont : Stmt.t list) : return * Mon.sl_label =
    let lbl s_eval = Mon.generate_label s s_eval in
    let (store, heap, stack, func) = state in
    Verbose.eval_small_step func s;
    match s.it with
    | Skip -> (Intermediate (state, cont), lbl SkipEval)
    | Merge -> (Intermediate (state, cont), lbl MergeEval)
    | Debug ->
      Debugger.run store heap stack;
      (Intermediate (state, cont), lbl DebugEval)
    | Block stmts -> (Intermediate (state, stmts @ cont), lbl BlockEval)
    | Print e ->
      eval_expr store e |> print_val heap;
      (Intermediate (state, cont), lbl PrintEval)
    | Return e -> (
      let v = eval_expr store e in
      let (frame, stack') = Call_stack.pop stack in
      match frame with
      | Call_stack.Toplevel _ -> (Final v, lbl ReturnEval)
      | Call_stack.Intermediate (cont', store', x, func') ->
        Store.set store' x v;
        let state' = (store', heap, stack', func') in
        (Intermediate (state', cont'), lbl ReturnEval) )
    | Assign (x, e) ->
      eval_expr store e |> Store.set store x;
      (Intermediate (state, cont), lbl AssignEval)
    | AssignCall (x, fe, es) -> (
      let (fn, fvs) = eval_func_expr store fe in
      let vs = fvs @ List.map (eval_expr store) es in
      match Mon.interceptor fn vs es with
      | Some lbl -> (Intermediate (state, cont), lbl)
      | None ->
        let func' = eval_func prog fn in
        let (stack', store') = prepare_call stack store cont x func' vs in
        let state' = (store', heap, stack', func') in
        let cont' = [ Func.body func' ] in
        (Intermediate (state', cont'), lbl (AssignCallEval func)) )
    | AssignECall (x, fe, es) ->
      let vs = List.map (eval_expr store) es in
      let v = External.execute prog heap fe vs in
      Store.set store x v;
      (Intermediate (state, cont), lbl AssignECallEval)
    | AssignNewObj x ->
      let l = Object.create () |> Heap.insert heap in
      Store.set store x (Val.Loc l);
      (Intermediate (state, cont), lbl (AssignNewObjEval l))
    | AssignObjToList (x, e) ->
      let fld_to_tup_f (fn, fv) = Val.Tuple [ Str fn; fv ] in
      let (_, obj) = eval_object store heap e in
      let v = Val.List (Object.fld_list obj |> List.map fld_to_tup_f) in
      Store.set store x v;
      (Intermediate (state, cont), lbl AssignObjToListEval)
    | AssignObjFields (x, e) ->
      let fld_to_tup_f (fn, _) = Val.Str fn in
      let (_, obj) = eval_object store heap e in
      let v = Val.List (Object.fld_list obj |> List.map fld_to_tup_f) in
      Store.set store x v;
      (Intermediate (state, cont), lbl AssignObjFieldsEval)
    | AssignInObjCheck (x, fe, oe) ->
      let in_obj = function Some _ -> true | None -> false in
      let (loc, obj) = eval_object store heap oe in
      let fn = eval_string store fe in
      let v = Val.Bool (Object.get obj fn |> in_obj) in
      Store.set store x v;
      (Intermediate (state, cont), lbl (AssignInObjCheckEval (loc, fn)))
    | FieldLookup (x, oe, fe) ->
      let fld_val v = Option.value ~default:(Val.Symbol "undefined") v in
      let (l, obj) = eval_object store heap oe in
      let fn = eval_string store fe in
      let v = Object.get obj fn |> fld_val in
      Store.set store x v;
      (Intermediate (state, cont), lbl (FieldLookupEval (l, fn)))
    | FieldAssign (oe, fe, e) ->
      let (l, obj) = eval_object store heap oe in
      let fn = eval_string store fe in
      let v = eval_expr store e in
      Object.set obj fn v;
      (Intermediate (state, cont), lbl (FieldAssignEval (l, fn)))
    | FieldDelete (oe, fe) ->
      let (l, obj) = eval_object store heap oe in
      let fn = eval_string store fe in
      Object.delete obj fn;
      (Intermediate (state, cont), lbl (FieldDeleteEval (l, fn)))
    | If (e, s1, s2) -> (
      let v = eval_boolean store e in
      let s2' = Option.value ~default:(Skip @> no_region) s2 in
      match (v, s1.it, s2'.it) with
      | (true, Block stmts, _) ->
        let cont' = stmts @ ((Stmt.Merge @> s1.at) :: cont) in
        (Intermediate (state, cont'), lbl (IfEval true))
      | (false, _, Block stmts) ->
        let cont' = stmts @ ((Stmt.Merge @> s2'.at) :: cont) in
        (Intermediate (state, cont'), lbl (IfEval false))
      | (false, _, Skip) -> (Intermediate (state, cont), lbl (IfEval false))
      | (true, _, _) -> Eslerr.internal __FUNCTION__ (Expecting "if block")
      | (false, _, _) -> Eslerr.internal __FUNCTION__ (Expecting "else block") )
    | While (e, s) ->
      let stmts = [ s; Stmt.While (e, s) @> s.at ] in
      let loop = Stmt.If (e, Stmt.Block stmts @> s.at, None) @> s.at in
      (Intermediate (state, loop :: cont), lbl WhileEval)
    | Fail e ->
      let v = eval_expr store e in
      (Error v, lbl FailEval)
    | Assert e ->
      let v = eval_boolean store e in
      if v then (Intermediate (state, cont), lbl (AssertEval true))
      else
        let err = Fmt.asprintf "Assert false: %a" Expr.pp e in
        (Error (Val.Str err), lbl (AssertEval false))
    | Abort _ -> (Intermediate (state, cont), lbl AbortEval)

  let eval_small_step_safe (prog : Prog.t) (state : state) (s : Stmt.t)
    (cont : Stmt.t list) : return * Mon.sl_label =
    try eval_small_step prog state s cont
    with Eslerr.Runtime_error _ as exn ->
      Eslerr.(set_loc (Stmt s) exn |> raise)

  let rec small_step_iter (prog : Prog.t) (state : state)
    (mon_state : Mon.state) (stmts : Stmt.t list) : return =
    match stmts with
    | [] ->
      let (_, _, _, func) = state in
      let fn = Func.name func in
      Eslerr.(runtime ~loc:(Func func) ~src:(Str fn) (MissingReturn fn))
    | s :: stmts' -> (
      let (return, lbl) = eval_small_step_safe prog state s stmts' in
      let mon_return = Mon.eval_small_step mon_state lbl in
      let mon_state' = Mon.next_state mon_return in
      match return with
      | Final v -> Final v
      | Error v -> Error v
      | Intermediate (state', stmts'') ->
        small_step_iter prog state' mon_state' stmts'' )

  let eval_prog ?(main : string = "main") (prog : Prog.t) : return =
    Verbose.init ();
    let func = eval_func prog main in
    let state = initial_state func in
    let mon_state = Mon.initial_state () in
    let return = small_step_iter prog state mon_state [ Func.body func ] in
    match return with
    | Final _ as retval -> retval
    | Error err as retval ->
      Fmt.eprintf "uncaught exception: %a@." Val.pp err;
      retval
    | _ -> Eslerr.internal __FUNCTION__ (Expecting "non-intermediate state")
end
