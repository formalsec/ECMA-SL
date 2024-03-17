open EslBase
open EslSyntax
open EslSyntax.Source

module Config = struct
  type t =
    { main : string
    ; resolve_exitval : bool
    ; static_heap : Val.t Heap.t option
    }

  let default : t =
    { main = "main"; resolve_exitval = true; static_heap = None }
end

module M (Db : Debugger.M) (Vb : Verbose.M) (Mon : Monitor.M) = struct
  type value = Val.t
  type obj = value Object.t
  type store = value Store.t
  type heap = value Heap.t
  type stack = store Call_stack.t
  type state = store * heap * stack * Db.t

  type return =
    | Final of value
    | Error of value
    | Intermediate of state * Stmt.t list

  let get_var (store : store) (x : string) (at : region) : value =
    match Store.get_opt store x with
    | None -> Runtime_error.(throw ~src:(ErrSrc.region at) (UnknownVar x))
    | Some v -> v

  let get_loc (heap : heap) (l : Loc.t) : obj =
    match Heap.get_opt heap l with
    | None -> Internal_error.(throw __FUNCTION__ (Expecting "existing location"))
    | Some obj -> obj

  let get_func (prog : Prog.t) (fn : string) (at : region) : Func.t =
    match Prog.func_opt prog fn with
    | None -> Runtime_error.(throw ~src:(ErrSrc.region at) (UnknownFunc fn))
    | Some f -> f

  let set_global_var (store : store) (heap : heap) : unit =
    let open Compiler.Const in
    if Option.is_some (Heap.get_opt heap esl_globals_loc) then
      Store.set store esl_globals_obj (Val.Loc esl_globals_loc)

  let initial_state (main : Func.t) (s_heap : heap option) : state =
    let store = Store.create [] in
    let heap = Option.fold ~none:(Heap.create ()) ~some:Heap.extend s_heap in
    let stack = Call_stack.create main in
    let db = Db.initialize () in
    set_global_var store heap;
    (store, heap, stack, db)

  let operate (eval_op_fun : unit -> value) (es : Expr.t list) : value =
    try eval_op_fun ()
    with Runtime_error.Error _ as exn ->
      let e = Runtime_error.(src exn |> ErrSrc.index_to_el es) in
      Runtime_error.(set_src (ErrSrc.at e) exn |> raise)

  let print_val (heap : heap) (v : value) : unit =
    let open Fmt in
    match v with
    | Str s -> printf "%s@." s
    | Loc l -> printf "%a@." (Object.pp Val.pp) (get_loc heap l)
    | _ -> printf "%a@." Val.pp v

  let rec eval_expr' (store : store) (e : Expr.t) : value =
    match e.it with
    | Val v -> v
    | Var x -> get_var store x e.at
    | UnOpt (op, e') ->
      let v = eval_expr store e' in
      let eval_op_fun () = Eval_operator.eval_unopt op v in
      operate eval_op_fun [ e' ]
    | BinOpt (op, e1, e2) ->
      let v1 = eval_expr store e1 in
      let v2 = eval_expr store e2 in
      let eval_op_fun () = Eval_operator.eval_binopt op v1 v2 in
      operate eval_op_fun [ e1; e2 ]
    | TriOpt (op, e1, e2, e3) ->
      let v1 = eval_expr store e1 in
      let v2 = eval_expr store e2 in
      let v3 = eval_expr store e3 in
      let eval_op_fun () = Eval_operator.eval_triopt op v1 v2 v3 in
      operate eval_op_fun [ e1; e2; e3 ]
    | NOpt (op, es) ->
      let vs = List.map (eval_expr store) es in
      let eval_op_fun () = Eval_operator.eval_nopt op vs in
      operate eval_op_fun es
    | Curry (fe, es) -> (
      let fv = eval_expr store fe in
      let vs = List.map (eval_expr store) es in
      match fv with
      | Str fn -> Val.Curry (fn, vs)
      | _ -> Runtime_error.(throw ~src:(ErrSrc.at fe) (BadExpr ("curry", fv))) )
    | Symbolic (t, _) -> (
      Random.self_init ();
      match t with
      | Type.IntType -> Val.Int (Random.int 128)
      | Type.FltType -> Val.Flt (Random.float 128.0)
      | _ ->
        Internal_error.(throw __FUNCTION__ (NotImplemented (Some "symbolic"))) )

  and eval_expr (store : store) (e : Expr.t) : value =
    let v = eval_expr' store e in
    Vb.eval_expr_val e v;
    v

  let eval_string (store : store) (e : Expr.t) : string =
    match eval_expr store e with
    | Str s -> s
    | _ as v -> Runtime_error.(throw ~src:(ErrSrc.at e) (BadVal ("string", v)))

  let eval_boolean (store : store) (e : Expr.t) : bool =
    match eval_expr store e with
    | Bool b -> b
    | _ as v -> Runtime_error.(throw ~src:(ErrSrc.at e) (BadVal ("boolean", v)))

  let eval_location (store : store) (e : Expr.t) : Loc.t =
    match eval_expr store e with
    | Loc l -> l
    | _ as v -> Runtime_error.(throw ~src:(ErrSrc.at e) (BadVal ("location", v)))

  let eval_object (store : store) (heap : heap) (e : Expr.t) : Loc.t * obj =
    let l = eval_location store e in
    let obj = get_loc heap l in
    (l, obj)

  let eval_func_expr (store : store) (fe : Expr.t) : string * value list =
    match eval_expr store fe with
    | Val.Str fn -> (fn, [])
    | Val.Curry (fn, fvs) -> (fn, fvs)
    | _ as v -> Runtime_error.(throw ~src:(ErrSrc.at fe) (BadFuncId v))

  let prepare_store_binds (pxs : string list) (vs : Val.t list) (at : region) :
    (string * Val.t) list =
    try List.combine pxs vs
    with _ ->
      let (xpxs, nargs) = (List.length pxs, List.length vs) in
      Runtime_error.(throw ~src:(ErrSrc.region at) (BadNArgs (xpxs, nargs)))

  let prepare_call (stack : stack) (f : Func.t) (store : store)
    (cont : Stmt.t list) (x : string) (vs : value list) (at : region) :
    stack * store =
    let pxs = Func.params' f in
    let stack' = Call_stack.push stack f store cont x in
    let store' = Store.create (prepare_store_binds pxs vs at) in
    (stack', store')

  let eval_small_step (p : Prog.t) (state : state) (s : Stmt.t)
    (cont : Stmt.t list) : return * Mon.sl_label =
    let lbl s_eval = Mon.generate_label s s_eval in
    let (store, heap, stack, db) = state in
    let f = Call_stack.func stack in
    Call_stack.update stack s;
    Vb.eval_small_step f s;
    match s.it with
    | Skip -> (Intermediate (state, cont), lbl SkipEval)
    | Merge -> (Intermediate (state, cont), lbl MergeEval)
    | Debug s ->
      let (db', stack', cont') = Db.run state s cont in
      let state' = (store, heap, stack', db') in
      (Intermediate (state', cont'), lbl DebugEval)
    | Block ss -> (Intermediate (state, ss @ cont), lbl BlockEval)
    | Print e ->
      eval_expr store e |> print_val heap;
      (Intermediate (state, cont), lbl PrintEval)
    | Return e -> (
      let v = eval_expr store e in
      let (frame, stack') = Call_stack.pop stack in
      match frame with
      | Call_stack.Toplevel _ -> (Final v, lbl ReturnEval)
      | Call_stack.Intermediate (_, restore) ->
        let (store', cont', x) = Call_stack.restore restore in
        Store.set store' x v;
        let state' = (store', heap, stack', db) in
        (Intermediate (state', cont'), lbl ReturnEval) )
    | Assign (x, e) ->
      eval_expr store e |> Store.set store x.it;
      (Intermediate (state, cont), lbl AssignEval)
    | AssignCall (x, fe, es) -> (
      let (fn, fvs) = eval_func_expr store fe in
      let vs = fvs @ List.map (eval_expr store) es in
      match Mon.interceptor fn vs es with
      | Some lbl -> (Intermediate (state, cont), lbl)
      | None ->
        let f' = get_func p fn fe.at in
        let (stack', store') = prepare_call stack f' store cont x.it vs fe.at in
        let cont' = [ Func.body f' ] in
        let (db', stack'', cont'') = Db.custom_inject s db stack' cont' in
        let state' = (store', heap, stack'', db') in
        (Intermediate (state', cont''), lbl (AssignCallEval f)) )
    | AssignECall (x, fn, es) ->
      let vs = List.map (eval_expr store) es in
      let v = External.execute p store heap fn.it vs in
      Store.set store x.it v;
      (Intermediate (state, cont), lbl AssignECallEval)
    | AssignNewObj x ->
      let l = Loc.create () in
      Heap.set heap l (Object.create ());
      Store.set store x.it (Val.Loc l);
      (Intermediate (state, cont), lbl (AssignNewObjEval l))
    | AssignObjToList (x, e) ->
      let fld_to_tup_f (fn, fv) = Val.Tuple [ Str fn; fv ] in
      let (_, obj) = eval_object store heap e in
      let v = Val.List (Object.fld_lst obj |> List.map fld_to_tup_f) in
      Store.set store x.it v;
      (Intermediate (state, cont), lbl AssignObjToListEval)
    | AssignObjFields (x, e) ->
      let fld_to_tup_f (fn, _) = Val.Str fn in
      let (_, obj) = eval_object store heap e in
      let v = Val.List (Object.fld_lst obj |> List.map fld_to_tup_f) in
      Store.set store x.it v;
      (Intermediate (state, cont), lbl AssignObjFieldsEval)
    | AssignInObjCheck (x, fe, oe) ->
      let in_obj = function Some _ -> true | None -> false in
      let (loc, obj) = eval_object store heap oe in
      let fn = eval_string store fe in
      let v = Val.Bool (Object.get obj fn |> in_obj) in
      Store.set store x.it v;
      (Intermediate (state, cont), lbl (AssignInObjCheckEval (loc, fn)))
    | FieldLookup (x, oe, fe) ->
      let fld_val v = Option.value ~default:(Val.Symbol "undefined") v in
      let (l, obj) = eval_object store heap oe in
      let fn = eval_string store fe in
      let v = Object.get obj fn |> fld_val in
      Store.set store x.it v;
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
      let s2' = Option.value ~default:(Stmt.Skip @> no_region) s2 in
      match (v, s1.it, s2'.it) with
      | (true, Block ss, _) ->
        let cont' = ss @ ((Stmt.Merge @> s1.at) :: cont) in
        (Intermediate (state, cont'), lbl (IfEval true))
      | (false, _, Block ss) ->
        let cont' = ss @ ((Stmt.Merge @> s2'.at) :: cont) in
        (Intermediate (state, cont'), lbl (IfEval false))
      | (false, _, Skip) -> (Intermediate (state, cont), lbl (IfEval false))
      | (true, _, _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "if block"))
      | (false, _, _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "else block")) )
    | While (e, s') ->
      let loop = Stmt.If (e, Stmt.Block [ s'; s ] @> s'.at, None) @> s.at in
      (Intermediate (state, loop :: cont), lbl WhileEval)
    | Switch (e, css, dflt) -> (
      let v = eval_expr store e in
      match (Hashtbl.find_opt css v, dflt) with
      | (Some { it = Block ss; at }, _) | (None, Some { it = Block ss; at }) ->
        let cont' = ss @ ((Stmt.Merge @> at) :: cont) in
        (Intermediate (state, cont'), lbl (SwitchEval v))
      | (Some _, _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "switch block"))
      | (None, Some _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "sdflt block"))
      | (None, None) -> (Intermediate (state, cont), lbl (SwitchEval v)) )
    | Fail e ->
      let v = eval_expr store e in
      (Error v, lbl FailEval)
    | Assert e ->
      let v = eval_boolean store e in
      if v then (Intermediate (state, cont), lbl (AssertEval true))
      else
        let err = Fmt.asprintf "Assert false: %a" Expr.pp e in
        (Error (Val.Str err), lbl (AssertEval false))

  let eval_small_step_safe (prog : Prog.t) (state : state) (s : Stmt.t)
    (cont : Stmt.t list) : return * Mon.sl_label =
    try eval_small_step prog state s cont
    with Runtime_error.Error _ as exn ->
      let (_, _, stack, _) = state in
      Runtime_error.(set_trace stack exn |> raise)

  let rec small_step_iter (prog : Prog.t) (state : state) (mon : Mon.state)
    (ss : Stmt.t list) : return =
    match ss with
    | [] ->
      let (_, _, stack, _) = state in
      let fn = Func.name (Call_stack.func stack) in
      Runtime_error.(throw ~src:(ErrSrc.at fn) (MissingReturn fn))
    | s :: cont -> (
      let (return, lbl) = eval_small_step_safe prog state s cont in
      let mon' = Mon.eval_small_step mon lbl |> Mon.next_state in
      match return with
      | Final v -> Final v
      | Error v -> Error v
      | Intermediate (state', cont') -> small_step_iter prog state' mon' cont' )

  let resolve_exitval (retval : value) : value =
    match retval with
    | Val.Tuple [ Val.Bool false; retval' ] -> retval'
    | Val.Tuple [ Val.Bool true; err ] ->
      Runtime_error.(throw (UncaughtExn (Val.str err)))
    | _ -> Runtime_error.(throw (UnexpectedExitVal retval))

  let eval_partial (config : Config.t) (prog : Prog.t) : value * heap =
    let f = get_func prog config.main no_region in
    let state = initial_state f config.static_heap in
    let mon = Mon.initial_state () in
    let return = small_step_iter prog state mon [ Func.body f ] in
    let (_, heap, _, _) = state in
    match return with
    | Final retval when config.resolve_exitval -> (resolve_exitval retval, heap)
    | Final retval -> (retval, heap)
    | Error err -> Runtime_error.(throw (Failure (Val.str err)))
    | _ ->
      Internal_error.(throw __FUNCTION__ (Expecting "non-intermediate state"))

  let eval_prog ?(config : Config.t = Config.default) (prog : Prog.t) : value =
    fst (eval_partial config prog)
end
