open EslBase
open EslSyntax
open EslSyntax.Source

module EntryPoint = struct
  type t =
    { main : string
    ; static_heap : Val.t Heap.t option
    }

  let default : t = { main = "main"; static_heap = None }
end

module M (Instrument : Instrument.M) = struct
  type obj = Val.t Object.t
  type store = Val.t Store.t
  type heap = Val.t Heap.t
  type stack = store Call_stack.t

  type state =
    { store : store
    ; heap : heap
    ; stack : stack
    }

  type return =
    | Final of Val.t
    | Error of Val.t
    | Intermediate of state * Stmt.t list

  module Config = struct
    let print_depth : int option ref = ref None
    let resolve_exitval : bool ref = ref true
    let show_exitval : bool ref = ref false
  end

  let set_global_var (store : store) (heap : heap) : unit =
    let open Compiler.Const in
    if Option.is_some (Heap.get_opt heap esl_globals_loc) then
      Store.set store esl_globals_obj (Loc esl_globals_loc)

  let initial_state (fmain : Func.t) (s_heap : heap option) : state =
    let store = Store.create [] in
    let heap = Option.fold ~none:(Heap.create ()) ~some:Heap.extend s_heap in
    let stack = Call_stack.create fmain in
    set_global_var store heap;
    { store; heap; stack }

  let get_var (store : store) (x : string) (at : region) : Val.t =
    match Store.get_opt store x with
    | None -> Runtime_error.(throw ~src:(ErrSrc.region at) (UnknownVar x))
    | Some v -> v

  let get_loc (heap : heap) (l : Loc.t) : obj =
    match Heap.get_opt heap l with
    | None -> Internal_error.(throw __FUNCTION__ (Expecting "existing location"))
    | Some obj -> obj

  let get_func (p : Prog.t) (fn : string) (at : region) : Func.t =
    match Prog.func_opt p fn with
    | None -> Runtime_error.(throw ~src:(ErrSrc.region at) (UnknownFunc fn))
    | Some f -> f

  let eval_operator (eval_op_fun : unit -> Val.t) (es : Expr.t list) : Val.t =
    try eval_op_fun ()
    with Runtime_error.Error err ->
      let e = Runtime_error.(src err |> ErrSrc.index_to_el es) in
      Runtime_error.(set_src (ErrSrc.at e) err |> raise)

  let rec eval_expr' (state : state) (e : Expr.t) : Val.t =
    match e.it with
    | Val v -> v
    | Var x -> get_var state.store x e.at
    | UnOpt (op, e') ->
      let v = eval_expr state e' in
      let eval_op_fun () = Eval_operator.eval_unopt op v in
      eval_operator eval_op_fun [ e' ]
    | BinOpt (op, e1, e2) ->
      let v1 = eval_expr state e1 in
      let v2 = eval_expr state e2 in
      let eval_op_fun () = Eval_operator.eval_binopt op v1 v2 in
      eval_operator eval_op_fun [ e1; e2 ]
    | TriOpt (op, e1, e2, e3) ->
      let v1 = eval_expr state e1 in
      let v2 = eval_expr state e2 in
      let v3 = eval_expr state e3 in
      let eval_op_fun () = Eval_operator.eval_triopt op v1 v2 v3 in
      eval_operator eval_op_fun [ e1; e2; e3 ]
    | NOpt (op, es) ->
      let vs = List.map (eval_expr state) es in
      let eval_op_fun () = Eval_operator.eval_nopt op vs in
      eval_operator eval_op_fun es
    | Curry (fe, es) -> (
      let fv = eval_expr state fe in
      let vs = List.map (eval_expr state) es in
      match fv with
      | Str fn -> Val.Curry (fn, vs)
      | _ -> Runtime_error.(throw ~src:(ErrSrc.at fe) (BadExpr ("curry", fv))) )
    | Symbolic (t, _) -> (
      Random.self_init ();
      match t with
      | Type.IntType -> Val.Int (Random.int 128)
      | Type.FltType -> Val.Flt (Random.float 128.0)
      | _ ->
        let msg = "symbolic " ^ Type.str t in
        Internal_error.(throw __FUNCTION__ (NotImplemented msg)) )

  and eval_expr (state : state) (e : Expr.t) : Val.t =
    let v = eval_expr' state e in
    let lvl = Call_stack.level state.stack in
    Instrument.Tracer.trace_expr lvl e (state.heap, v);
    v

  let eval_string (state : state) (e : Expr.t) : string =
    match eval_expr state e with
    | Str s -> s
    | _ as v -> Runtime_error.(throw ~src:(ErrSrc.at e) (BadVal ("string", v)))

  let eval_boolean (state : state) (e : Expr.t) : bool =
    match eval_expr state e with
    | Bool b -> b
    | _ as v -> Runtime_error.(throw ~src:(ErrSrc.at e) (BadVal ("boolean", v)))

  let eval_location (state : state) (e : Expr.t) : Loc.t =
    match eval_expr state e with
    | Loc l -> l
    | _ as v -> Runtime_error.(throw ~src:(ErrSrc.at e) (BadVal ("location", v)))

  let eval_object (state : state) (heap : heap) (e : Expr.t) : Loc.t * obj =
    let l = eval_location state e in
    let obj = get_loc heap l in
    (l, obj)

  let eval_func_expr (state : state) (fe : Expr.t) : string * Val.t list =
    match eval_expr state fe with
    | Val.Str fn -> (fn, [])
    | Val.Curry (fn, fvs) -> (fn, fvs)
    | _ as v -> Runtime_error.(throw ~src:(ErrSrc.at fe) (BadFuncId v))

  let rec heapval_pp (depth : int option) (visited : (Loc.t, unit) Hashtbl.t)
    (heap : heap) (fmt : Fmt.t) (v : Val.t) : unit =
    let valid_depth = Option.fold ~none:true ~some:(fun d -> d > 0) in
    match v with
    | Loc l when valid_depth depth && not (Hashtbl.mem visited l) ->
      let depth' = Option.map (fun d -> d - 1) depth in
      Hashtbl.add visited l ();
      (Object.pp (heapval_pp depth' visited heap)) fmt (get_loc heap l)
    | Loc _ -> Fmt.fprintf fmt "{...}"
    | _ -> Val.pp fmt v

  let print_pp (heap : heap) (fmt : Fmt.t) (v : Val.t) : unit =
    match v with
    | Str s -> Fmt.pp_str fmt s
    | _ ->
      let visited = Hashtbl.create !Base.default_hashtbl_sz in
      heapval_pp !Config.print_depth visited heap fmt v

  let prepare_store_binds (pxs : string list) (vs : Val.t list) (at : region) :
    (string * Val.t) list =
    try List.combine pxs vs
    with _ ->
      let (xpxs, nargs) = (List.length pxs, List.length vs) in
      Runtime_error.(throw ~src:(ErrSrc.region at) (BadNArgs (xpxs, nargs)))

  let prepare_call (stack : stack) (f : Func.t) (store : store)
    (cont : Stmt.t list) (x : string) (vs : Val.t list) (at : region) :
    stack * store =
    let pxs = Func.params' f in
    let stack' = Call_stack.push stack f store cont x in
    let store' = Store.create (prepare_store_binds pxs vs at) in
    (stack', store')

  let update_mon_label (inst : Instrument.t ref) (s : Stmt.t)
    (s_eval : Monitor.stmt_eval) : unit =
    let mon_label = Instrument.Monitor.generate_label s s_eval in
    inst := { !inst with mon_label }

  let eval_small_step (p : Prog.t) (state : state) (inst : Instrument.t ref)
    (s : Stmt.t) (cont : Stmt.t list) : return * unit =
    let lbl s_eval = update_mon_label inst s s_eval in
    let lvl = Call_stack.level state.stack in
    Instrument.Tracer.trace_stmt lvl s;
    match s.it with
    | Skip -> (Intermediate (state, cont), lbl SkipEval)
    | Merge -> (Intermediate (state, cont), lbl MergeEval)
    | Debug s' ->
      let db_state = (state.store, state.heap, state.stack) in
      let db_res = Instrument.Debugger.run !inst.db db_state cont s' in
      let (db', (store, heap, stack), cont') = db_res in
      inst := { !inst with db = db' };
      (Intermediate ({ store; heap; stack }, s' :: cont'), lbl DebugEval)
    | Block ss -> (Intermediate (state, ss @ cont), lbl BlockEval)
    | Print e ->
      eval_expr state e |> Log.out "%a@." (print_pp state.heap);
      (Intermediate (state, cont), lbl PrintEval)
    | Return e -> (
      let v = eval_expr state e in
      let f = Call_stack.func state.stack in
      Instrument.Tracer.trace_return lvl f s (state.heap, v);
      let (frame, stack') = Call_stack.pop state.stack in
      match frame with
      | Call_stack.Toplevel _ -> (Final v, lbl ReturnEval)
      | Call_stack.Intermediate (_, restore) ->
        let (store', cont', x) = Call_stack.restore restore in
        let state' = { state with store = store'; stack = stack' } in
        Store.set store' x v;
        Instrument.Tracer.trace_restore (lvl - 1) (Call_stack.func stack');
        (Intermediate (state', cont'), lbl ReturnEval) )
    | Assign (x, e) ->
      eval_expr state e |> Store.set state.store x.it;
      (Intermediate (state, cont), lbl AssignEval)
    | AssignCall (x, fe, es) -> (
      let (fn, fvs) = eval_func_expr state fe in
      let vs = fvs @ List.map (eval_expr state) es in
      match Instrument.Monitor.interceptor fn vs es with
      | Some lbl ->
        inst := { !inst with mon_label = lbl };
        (Intermediate (state, cont), ())
      | None ->
        let f = get_func p fn fe.at in
        let (stack, store) = (state.stack, state.store) in
        let (stack', store') = prepare_call stack f store cont x.it vs fe.at in
        let cont' = [ Func.body f ] in
        let db_res = Instrument.Debugger.call !inst.db stack' cont' in
        let (db', stack'', cont'') = db_res in
        let state' = { state with store = store'; stack = stack'' } in
        inst := { !inst with db = db' };
        Instrument.Tracer.trace_call (lvl + 1) f s;
        (Intermediate (state', cont''), lbl (AssignCallEval f)) )
    | AssignECall (x, fn, es) ->
      let vs = List.map (eval_expr state) es in
      let v = External.execute p state.store state.heap fn.it vs in
      Store.set state.store x.it v;
      (Intermediate (state, cont), lbl AssignECallEval)
    | AssignNewObj x ->
      let l = Loc.create () in
      Heap.set state.heap l (Object.create ());
      Store.set state.store x.it (Val.Loc l);
      (Intermediate (state, cont), lbl (AssignNewObjEval l))
    | AssignObjToList (x, e) ->
      let fld_to_tup_f (fn, fv) = Val.Tuple [ Str fn; fv ] in
      let (_, obj) = eval_object state state.heap e in
      let v = Val.List (Object.fld_lst obj |> List.map fld_to_tup_f) in
      Store.set state.store x.it v;
      (Intermediate (state, cont), lbl AssignObjToListEval)
    | AssignObjFields (x, e) ->
      let fld_to_tup_f (fn, _) = Val.Str fn in
      let (_, obj) = eval_object state state.heap e in
      let v = Val.List (Object.fld_lst obj |> List.map fld_to_tup_f) in
      Store.set state.store x.it v;
      (Intermediate (state, cont), lbl AssignObjFieldsEval)
    | AssignInObjCheck (x, fe, oe) ->
      let in_obj = function Some _ -> true | None -> false in
      let (loc, obj) = eval_object state state.heap oe in
      let fn = eval_string state fe in
      let v = Val.Bool (Object.get obj fn |> in_obj) in
      Store.set state.store x.it v;
      (Intermediate (state, cont), lbl (AssignInObjCheckEval (loc, fn)))
    | FieldLookup (x, oe, fe) ->
      let fld_val v = Option.value ~default:(Val.Symbol "undefined") v in
      let (l, obj) = eval_object state state.heap oe in
      let fn = eval_string state fe in
      let v = Object.get obj fn |> fld_val in
      Store.set state.store x.it v;
      (Intermediate (state, cont), lbl (FieldLookupEval (l, fn)))
    | FieldAssign (oe, fe, e) ->
      let (l, obj) = eval_object state state.heap oe in
      let fn = eval_string state fe in
      let v = eval_expr state e in
      Object.set obj fn v;
      (Intermediate (state, cont), lbl (FieldAssignEval (l, fn)))
    | FieldDelete (oe, fe) ->
      let (l, obj) = eval_object state state.heap oe in
      let fn = eval_string state fe in
      Object.delete obj fn;
      (Intermediate (state, cont), lbl (FieldDeleteEval (l, fn)))
    | If (e, s1, s2) -> (
      let v = eval_boolean state e in
      let s2' = Option.value ~default:(Stmt.Skip @> no_region) s2 in
      match (v, s1.it, s2'.it) with
      | (true, Block ss, _) ->
        let cont' = ss @ ((Stmt.Merge @?> s1.at) :: cont) in
        (Intermediate (state, cont'), lbl (IfEval true))
      | (false, _, Block ss) ->
        let cont' = ss @ ((Stmt.Merge @?> s2'.at) :: cont) in
        (Intermediate (state, cont'), lbl (IfEval false))
      | (false, _, Skip) -> (Intermediate (state, cont), lbl (IfEval false))
      | (true, _, _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "if block"))
      | (false, _, _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "else block")) )
    | While (e, s') ->
      let loop = Stmt.If (e, Stmt.Block [ s'; s ] @?> s'.at, None) @?> s.at in
      (Intermediate (state, loop :: cont), lbl WhileEval)
    | Switch (e, css, dflt) -> (
      let v = eval_expr state e in
      match (Hashtbl.find_opt css v, dflt) with
      | (Some { it = Block ss; at }, _) | (None, Some { it = Block ss; at }) ->
        let cont' = ss @ ((Stmt.Merge @?> at) :: cont) in
        (Intermediate (state, cont'), lbl (SwitchEval v))
      | (Some _, _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "switch block"))
      | (None, Some _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "sdflt block"))
      | (None, None) -> (Intermediate (state, cont), lbl (SwitchEval v)) )
    | Fail e ->
      let v = eval_expr state e in
      (Error v, lbl FailEval)
    | Assert e ->
      let v = eval_boolean state e in
      if v then (Intermediate (state, cont), lbl (AssertEval true))
      else
        let err = Fmt.asprintf "Assert false: %a" Expr.pp e in
        (Error (Val.Str err), lbl (AssertEval false))

  let eval_small_step_safe (p : Prog.t) (state : state)
    (inst : Instrument.t ref) (s : Stmt.t) (cont : Stmt.t list) : return =
    let state' = { state with stack = Call_stack.update state.stack s } in
    try fst (eval_small_step p state' inst s cont)
    with Runtime_error.Error err ->
      Runtime_error.(set_trace state.stack err |> raise)

  let rec small_step_iter (p : Prog.t) (state : state) (inst : Instrument.t ref)
    (ss : Stmt.t list) : return =
    match ss with
    | [] ->
      let fn = Func.name (Call_stack.func state.stack) in
      Runtime_error.(throw ~src:(ErrSrc.at fn) (MissingReturn fn))
    | s :: cont -> (
      let return = eval_small_step_safe p state inst s cont in
      let module Mon = Instrument.Monitor in
      let mon_return = Mon.eval_small_step !inst.mon_state !inst.mon_label in
      let mon_state' = Mon.next_state mon_return in
      inst := { !inst with mon_state = mon_state' };
      match return with
      | Final v -> Final v
      | Error v -> Error v
      | Intermediate (state', cont') -> small_step_iter p state' inst cont' )

  let resolve_exitval (retval : Val.t) : Val.t =
    if not !Config.resolve_exitval then retval
    else
      match retval with
      | Val.Tuple [ Val.Bool false; retval' ] -> retval'
      | Val.Tuple [ Val.Bool true; err ] ->
        Runtime_error.(throw (UncaughtExn (Val.str err)))
      | _ -> Runtime_error.(throw (UnexpectedExitVal retval))

  let show_exitval (retval : Val.t) : unit =
    if !Config.show_exitval then Log.esl ~nl:true "exit value: %a" Val.pp retval

  let eval_exitval (retval : Val.t) : Val.t =
    let retval' = resolve_exitval retval in
    show_exitval retval';
    retval'

  let eval_instrumented (entry : EntryPoint.t) (p : Prog.t)
    (inst : Instrument.t ref) : Val.t * heap =
    let fmain = get_func p entry.main no_region in
    let state = initial_state fmain entry.static_heap in
    Instrument.Tracer.trace_restore (-1) fmain;
    let return = small_step_iter p state inst [ Func.body fmain ] in
    match return with
    | Final v -> (eval_exitval v, state.heap)
    | Error err -> Runtime_error.(throw (Failure (Val.str err)))
    | _ -> Internal_error.(throw __FUNCTION__ (Unexpected "intermediate state"))

  let eval_partial (entry : EntryPoint.t) (p : Prog.t) : Val.t * heap =
    let inst = ref (Instrument.initial_state ()) in
    let eval_expr (store, heap, stack) = eval_expr { store; heap; stack } in
    let heapval_pp = heapval_pp !Config.print_depth in
    Instrument.Debugger.set_interp_callbacks { heapval_pp; eval_expr };
    let execute () = eval_instrumented entry p inst in
    let finally () = Instrument.cleanup !inst in
    Fun.protect ~finally execute

  let eval_prog ?(entry : EntryPoint.t = EntryPoint.default) (p : Prog.t) :
    Val.t =
    fst (eval_partial entry p)
end
