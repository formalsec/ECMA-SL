include Interpreter_base
open EslBase
open EslSyntax
open EslSyntax.Source

module M (ITooling : Interpreter_tooling.M) = struct
  type st =
    { store : store
    ; heap : heap
    ; stack : stack
    ; itool : ITooling.t ref
    }

  type return =
    | Final of Value.t
    | Error of Value.t
    | Intermediate of st * Stmt.t list

  let set_global_var (store : store) (heap : heap) : unit =
    let open Compiler.Const in
    if Option.is_some (Heap.get heap IConst.global_loc) then
      Store.set store esl_globals_obj (Value.loc IConst.global_loc)

  let initial_state (heap' : heap option) (itool : ITooling.t ref)
    (main : Func.t) : st =
    let store = Store.create [] in
    let heap = Option.fold ~none:(Heap.create ()) ~some:Heap.extend heap' in
    let stack = Call_stack.create main in
    set_global_var store heap;
    { store; heap; stack; itool }

  let get_obj (heap : heap) (l : Loc.t) : obj =
    match Heap.get heap l with
    | None -> Log.fail "expecting existing location, but got %a" Loc.pp l
    | Some obj -> obj

  let get_var (store : store) (x : string) (at : at) : Value.t =
    match Store.get store x with
    | None -> Runtime_error.(throw ~src:at (UnknownVar x))
    | Some v -> v

  let get_func (p : Prog.t) (fn : string) (at : at) : Func.t =
    match Prog.func p fn with
    | None -> Runtime_error.(throw ~src:at (UnknownFunc fn))
    | Some f -> f

  let eval_op_semantics (op_lbl_f : unit -> string) (op_eval_f : unit -> Value.t)
    : Value.t =
    try op_eval_f () with
    | Runtime_error.Error err ->
      Runtime_error.(push (OpEvalExn (op_lbl_f ())) err |> raise)
    | err -> Log.fail "unexpected operator error: %s" (Printexc.to_string err)

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
    Instrument.Profiler.count !(state.inst).pf `Expr;
    Instrument.Tracer.trace_expr !(state.inst).tr e (state.heap, v);
    v

  and eval_expr' (st : st) (e : Expr.t) : Value.t =
    match e.it with
    | Val v -> v
    | Var x -> get_var st.store x e.at
    | UnOpt (op, e') ->
      let arg = (eval_expr st e', e'.at) in
      let op_lbl_f () = Operator.unopt_label op in
      let op_eval_f () = Eval_op.unopt_semantics op arg in
      eval_op_semantics op_lbl_f op_eval_f
    | BinOpt (op, e1, e2) ->
      let arg1 = (eval_expr st e1, e1.at) in
      let arg2 = (eval_expr st e2, e2.at) in
      let op_lbl_f () = Operator.binopt_label op in
      let op_eval_f () = Eval_op.binopt_semantics op (arg1, arg2) in
      eval_op_semantics op_lbl_f op_eval_f
    | TriOpt (op, e1, e2, e3) ->
      let arg1 = (eval_expr st e1, e1.at) in
      let arg2 = (eval_expr st e2, e2.at) in
      let arg3 = (eval_expr st e3, e3.at) in
      let op_lbl_f () = Operator.triopt_label op in
      let op_eval_f () = Eval_op.triopt_semantics op (arg1, arg2, arg3) in
      eval_op_semantics op_lbl_f op_eval_f
    | NOpt (op, es) ->
      let args = List.map (fun e -> (eval_expr st e, e.at)) es in
      let op_lbl_f () = Operator.nopt_label op in
      let op_eval_f () = Eval_op.nopt_semantics op args in
      eval_op_semantics op_lbl_f op_eval_f
    | Curry (fe, es) -> (
      let fv = eval_expr st fe in
      let vs = List.map (eval_expr st) es in
      match fv with
      | Str fn -> Value.App (`Op fn, vs)
      | _ -> Runtime_error.(throw ~src:fe.at (BadExpr ("curry", fv))) )

  let eval_str (st : st) (e : Expr.t) : string =
    match eval_expr st e with
    | Str s -> s
    | _ as v -> Runtime_error.(throw ~src:e.at (BadVal ("string", v)))

  let eval_bool (st : st) (e : Expr.t) : bool =
    match eval_expr st e with
    | Value.True -> true
    | Value.False -> false
    | _ as v -> Runtime_error.(throw ~src:e.at (BadVal ("boolean", v)))

  let eval_loc (st : st) (e : Expr.t) : Loc.t =
    match eval_expr st e with
    | App (`Op "loc", [ Int l ]) -> l
    | _ as v -> Runtime_error.(throw ~src:e.at (BadVal ("location", v)))

  let eval_func_expr (st : st) (fe : Expr.t) : string * Value.t list =
    match eval_expr st fe with
    | Value.Str fn -> (fn, [])
    | Value.App (`Op fn, fvs) -> (fn, fvs)
    | _ as v -> Runtime_error.(throw ~src:fe.at (BadFuncId v))

  let eval_obj (st : st) (e : Expr.t) : Loc.t * obj =
    let loc = eval_loc st e in
    let obj = get_obj st.heap loc in
    (loc, obj)

  let rec rec_print_pp (depth : int option) (visited : (Loc.t, unit) Hashtbl.t)
    (heap : heap) (ppf : Fmt.t) (v : Value.t) : unit =
    let inv_depth_f = Option.fold ~none:false ~some:(fun d -> d <= 0) in
    let incr_depth_f = Option.map (fun d -> d - 1) in
    let visited_loc_f = Hashtbl.mem visited in
    let rec_print_pp' = rec_print_pp (incr_depth_f depth) visited heap in
    match v with
    | List _ when inv_depth_f depth -> Fmt.fmt ppf "[...]"
    | App (`Op "loc", [ Int l ]) when inv_depth_f depth || visited_loc_f l ->
      Fmt.fmt ppf "{...}"
    | App (`Op "loc", [ Int l ]) ->
      Hashtbl.add visited l ();
      (Object.pp rec_print_pp') ppf (get_obj heap l);
      Hashtbl.remove visited l
    | _ -> Value.pp_custom_val rec_print_pp' ppf v

  let print_pp (heap : heap) (ppf : Fmt.t) (v : Value.t) : unit =
    let mk_visited () = Hashtbl.create !Base.default_hashtbl_sz in
    match v with
    | Str s -> Fmt.pp_str ppf s
    | _ -> rec_print_pp !IConfig.print_depth (mk_visited ()) heap ppf v

  let prepare_store_binds (pxs : string list) (vs : Value.t list) (at : at) :
    (string * Value.t) list =
    try List.combine pxs vs
    with Invalid_argument _ ->
      let (npxs, nargs) = (List.length pxs, List.length vs) in
      Runtime_error.(throw ~src:at (BadNArgs (npxs, nargs)))

  let prepare_call (st : st) (f : Func.t) (cont : Stmt.t list) (x : string)
    (vs : Value.t list) (at : at) : stack * store =
    let pxs = Func.params' f in
    let stack' = Call_stack.push st.stack f st.store cont x in
    let store' = Store.create (prepare_store_binds pxs vs at) in
    (stack', store')

  let eval_small_step (p : Prog.t) (state : state) (s : Stmt.t)
    (cont : Stmt.t list) : return =
    let inst = !(state.inst) in
    let lbl_f = Instrument.Monitor.update_label in
    let ( $$ ) v s_eval = lbl_f inst.mon s s_eval |> fun () -> v in
    Instrument.Tracer.trace_stmt inst.tr s;
    Instrument.Profiler.count inst.pf `Stmt;
    match s.it with
    | Skip -> Intermediate (st, cont) $$ SkipEval
    | Merge -> Intermediate (st, cont) $$ MergeEval
    | Debug s' ->
      let db_st = (st.store, st.heap, st.stack) in
      let db_res = ITooling.Debugger.run itool.db db_st cont s' in
      let ((store, heap, stack), cont') = db_res in
      Intermediate ({ st with store; heap; stack }, s' :: cont') $$ DebugEval
    | Block ss -> Intermediate (st, ss @ cont) $$ BlockEval
    | Print e ->
      Log.stdout "%a@." (print_pp st.heap) (eval_expr st e);
      Intermediate (st, cont) $$ PrintEval
    | Return e -> (
      let v = eval_expr state e in
      let f = Call_stack.func state.stack in
      Instrument.Tracer.trace_return inst.tr f s (state.heap, v);
      let (frame, stack') = Call_stack.pop state.stack in
      match frame with
      | Call_stack.Toplevel _ -> Final v $$ ReturnEval
      | Call_stack.Intermediate (_, restore) ->
        let (store', cont', x) = Call_stack.restore restore in
        ITooling.Tracer.trace_restore (lvl - 1) (Call_stack.func stack');
        Store.set store' x v;
        Instrument.Tracer.trace_restore inst.tr (Call_stack.func stack');
        Intermediate (state', cont') $$ ReturnEval )
    | Assign (x, e) ->
      Store.set state.store x.it (eval_expr state e);
      Intermediate (state, cont) $$ AssignEval
    | AssignCall (x, fe, es) -> (
      let (fn, fvs) = eval_func_expr state fe in
      let vs = fvs @ List.map (eval_expr state) es in
      match Instrument.Monitor.interceptor fn vs es with
      | Some lbl ->
        Instrument.Monitor.set_label inst.mon lbl;
        Intermediate (state, cont)
      | None ->
        let f = get_func p fn fe.at in
        let (stack, store) = (state.stack, state.store) in
        let (stack', store') = prepare_call stack f store cont x.it vs fe.at in
        let cont' = [ Func.body f ] in
        let db_res = Instrument.Debugger.call inst.db stack' cont' in
        let (stack'', cont'') = db_res in
        let state' = { state with store = store'; stack = stack'' } in
        Instrument.Profiler.count inst.pf `Call;
        Instrument.Tracer.trace_call inst.tr f s;
        Intermediate (state', cont'') $$ AssignCallEval f )
    | AssignECall (x, fn, es) ->
      let vs = List.map (eval_expr state) es in
      let v = External.execute p state.store state.heap fn.it vs in
      Store.set state.store x.it v;
      Intermediate (state, cont) $$ AssignECallEval
    | AssignNewObj x ->
      let l = Loc.create () in
      Heap.set state.heap l (Object.create ());
      Store.set state.store x.it (Val.Loc l);
      Intermediate (state, cont) $$ AssignNewObjEval l
    | AssignObjToList (x, e) ->
      let fld_to_tup_f (fn, fv) = Val.Tuple [ Str fn; fv ] in
      let (_, obj) = eval_obj state state.heap e in
      let v = Val.List (Object.fld_lst obj |> List.map fld_to_tup_f) in
      Store.set state.store x.it v;
      Intermediate (state, cont) $$ AssignObjToListEval
    | AssignObjFields (x, e) ->
      let fld_to_tup_f (fn, _) = Val.Str fn in
      let (_, obj) = eval_obj state state.heap e in
      let v = Val.List (Object.fld_lst obj |> List.map fld_to_tup_f) in
      Store.set state.store x.it v;
      Intermediate (state, cont) $$ AssignObjFieldsEval
    | AssignInObjCheck (x, fe, oe) ->
      let in_obj = function Some _ -> true | None -> false in
      let (loc, obj) = eval_obj state state.heap oe in
      let fn = eval_str state fe in
      let v = Val.Bool (Object.get obj fn |> in_obj) in
      Store.set state.store x.it v;
      Intermediate (state, cont) $$ AssignInObjCheckEval (loc, fn)
    | FieldLookup (x, oe, fe) ->
      let fld_val v = Option.value ~default:(Val.Symbol "undefined") v in
      let (l, obj) = eval_obj state state.heap oe in
      let fn = eval_str state fe in
      let v = Object.get obj fn |> fld_val in
      Store.set state.store x.it v;
      Intermediate (state, cont) $$ FieldLookupEval (l, fn)
    | FieldAssign (oe, fe, e) ->
      let (l, obj) = eval_obj state state.heap oe in
      let fn = eval_str state fe in
      let v = eval_expr state e in
      Object.set obj fn v;
      Intermediate (state, cont) $$ FieldAssignEval (l, fn)
    | FieldDelete (oe, fe) ->
      let (l, obj) = eval_obj state state.heap oe in
      let fn = eval_str state fe in
      Object.delete obj fn;
      Intermediate (state, cont) $$ FieldDeleteEval (l, fn)
    | If (e, s1, s2) -> (
      let v = eval_bool state e in
      let s2' = Option.value ~default:(Stmt.Skip @> no_region) s2 in
      match (v, s1.it, s2'.it) with
      | (true, Block ss, _) ->
        let cont' = ss @ ((Stmt.Merge @?> s1.at) :: cont) in
        Intermediate (state, cont') $$ IfEval true
      | (false, _, Block ss) ->
        let cont' = ss @ ((Stmt.Merge @?> s2'.at) :: cont) in
        Intermediate (state, cont') $$ IfEval false
      | (false, _, Skip) -> Intermediate (state, cont) $$ IfEval false
      | (true, _, _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "if block"))
      | (false, _, _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "else block")) )
    | While (e, s') ->
      let loop = Stmt.If (e, Stmt.Block [ s'; s ] @?> s'.at, None) @?> s.at in
      Intermediate (state, loop :: cont) $$ WhileEval
    | Switch (e, css, dflt) -> (
      let v = eval_expr state e in
      match (Hashtbl.find_opt css v, dflt) with
      | (Some { it = Block ss; at }, _) | (None, Some { it = Block ss; at }) ->
        let cont' = ss @ ((Stmt.Merge @?> at) :: cont) in
        Intermediate (state, cont') $$ SwitchEval v
      | (Some _, _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "switch block"))
      | (None, Some _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "sdflt block"))
      | (None, None) -> Intermediate (state, cont) $$ SwitchEval v )
>>>>>>> 3d2021e73 (refactor(tracer): add function level as the tracer's internal state)
    | Fail e ->
      let v = eval_expr st e in
      Error v $$ FailEval
    | Assert e ->
      let assert_err e = Value.Str (Fmt.str "Assert false: %a" Expr.pp e) in
      let v = eval_bool st e in
      if v then Intermediate (st, cont) $$ AssertEval true
      else Error (assert_err e) $$ AssertEval false
    | Assign (x, e) ->
      let v = eval_expr st e in
      Store.set st.store x.it v;
      Intermediate (st, cont) $$ AssignEval
    | AssignCall (x, fe, es) -> (
      let (fn, fvs) = eval_func_expr st fe in
      let vs = fvs @ List.map (eval_expr st) es in
      match ITooling.Monitor.interceptor fn vs es with
      | Some lbl ->
        ITooling.Monitor.set_label itool.mon lbl;
        Intermediate (st, cont)
      | None ->
        let f = get_func p fn fe.at in
        let (stack', store') = prepare_call st f cont x.it vs fe.at in
        let cont' = [ Func.body f ] in
        let db_res = ITooling.Debugger.call itool.db stack' cont' in
        let (stack'', cont'') = db_res in
        let st' = { st with store = store'; stack = stack'' } in
        ITooling.Profiler.count itool.pf `Call;
        ITooling.Tracer.trace_call (lvl + 1) f s;
        Intermediate (st', cont'') $$ AssignCallEval f )
    | AssignECall (x, fn, es) ->
      let vs = List.map (eval_expr st) es in
      let v = External.execute p st.store st.heap fn.it vs in
      Store.set st.store x.it v;
      Intermediate (st, cont) $$ AssignECallEval
    | AssignNewObj x ->
      let loc = Loc.create () in
      let obj = Object.create () in
      Heap.set st.heap loc obj;
      Store.set st.store x.it (Value.loc loc);
      Intermediate (st, cont) $$ AssignNewObjEval loc
    | AssignObjToList (x, e) ->
      let format_fld (fn, fv) = Value.List [ Str fn; fv ] in
      let (_, obj) = eval_obj st e in
      let v = Value.List (Object.fld_lst obj |> List.map format_fld) in
      Store.set st.store x.it v;
      Intermediate (st, cont) $$ AssignObjToListEval
    | AssignObjFields (x, e) ->
      let format_fld (fn, _) = Value.Str fn in
      let (_, obj) = eval_obj st e in
      let v = Value.List (Object.fld_lst obj |> List.map format_fld) in
      Store.set st.store x.it v;
      Intermediate (st, cont) $$ AssignObjFieldsEval
    | AssignInObjCheck (x, fe, oe) ->
      let in_obj = function Some _ -> Value.True | None -> Value.False in
      let (loc, obj) = eval_obj st oe in
      let fn = eval_str st fe in
      let v = Object.get obj fn |> in_obj in
      Store.set st.store x.it v;
      Intermediate (st, cont) $$ AssignInObjCheckEval (loc, fn)
    | FieldLookup (x, oe, fe) ->
      let fld_val v = Option.value ~default:Value.undefined v in
      let (loc, obj) = eval_obj st oe in
      let fn = eval_str st fe in
      let v = Object.get obj fn |> fld_val in
      Store.set st.store x.it v;
      Intermediate (st, cont) $$ FieldLookupEval (loc, fn)
    | FieldAssign (oe, fe, e) ->
      let (loc, obj) = eval_obj st oe in
      let fn = eval_str st fe in
      let v = eval_expr st e in
      Object.set obj fn v;
      Intermediate (st, cont) $$ FieldAssignEval (loc, fn)
    | FieldDelete (oe, fe) ->
      let (loc, obj) = eval_obj st oe in
      let fn = eval_str st fe in
      Object.delete obj fn;
      Intermediate (st, cont) $$ FieldDeleteEval (loc, fn)
    | If (e, s1, s2) -> (
      let v = eval_bool st e in
      let s2' = Option.value ~default:(Stmt.Skip @?> none) s2 in
      match (v, s1.it, s2'.it) with
      | (true, Block ss, _) | (false, _, Block ss) ->
        let cont' = ss @ ((Stmt.Merge @?> none) :: cont) in
        Intermediate (st, cont') $$ IfEval v
      | (false, _, Skip) -> Intermediate (st, cont) $$ IfEval false
      | (true, _, _) -> Log.fail "expecting if block, but got %a" Stmt.pp s1
      | (false, _, _) -> Log.fail "expecting else block, but got %a" Stmt.pp s2'
      )
    | While (e, s') ->
      let loop = Stmt.If (e, Stmt.Block [ s'; s ] @?> s'.at, None) @?> s.at in
      Intermediate (st, loop :: cont) $$ WhileEval
    | Switch (e, css, dflt) -> (
      let v = eval_expr st e in
      match (Hashtbl.find_opt css v, dflt) with
      | (Some { it = Block ss; _ }, _) | (None, Some { it = Block ss; _ }) ->
        let cont' = ss @ ((Stmt.Merge @?> none) :: cont) in
        Intermediate (st, cont') $$ SwitchEval v
      | (None, None) -> Intermediate (st, cont) $$ SwitchEval v
      | (Some s1', _) ->
        Log.fail "expecting switch block, but got %a" Stmt.pp s1'
      | (None, Some s2') ->
        Log.fail "expecting default block, but got %a" Stmt.pp s2' )

  let eval_small_step_safe (p : Prog.t) (st : st) (s : Stmt.t)
    (cont : Stmt.t list) : return =
    let st' = { st with stack = Call_stack.update st.stack s } in
    try eval_small_step p st' s cont
    with Runtime_error.Error err ->
      Runtime_error.(set_trace st.stack err |> raise)

  let rec small_step_iter (p : Prog.t) (st : st) (cont : Stmt.t list) : return =
    match cont with
    | [] ->
      let fn = Func.name (Call_stack.func st.stack) in
      Runtime_error.(throw ~src:fn.at (MissingReturn fn))
    | s :: cont' -> (
      let return = eval_small_step_safe p st s cont' in
      ITooling.Monitor.eval_small_step !(st.itool).mon;
      match return with
      | Intermediate (st'', cont'') -> small_step_iter p st'' cont''
      | _ -> return )

  let resolve_exitval (retval : Val.t) : Val.t =
    if not !Config.resolve_exitval then retval
    else
      match retval with
      | Val.Tuple [ Val.Bool false; retval' ] -> retval'
      | Val.Tuple [ Val.Bool true; err ] ->
        Runtime_error.(throw (UncaughtExn (Val.str err)))
      | _ -> Runtime_error.(throw (UnexpectedExitVal retval))

  let show_exitval (heap : heap) (retval : Val.t) : unit =
    let visited = Hashtbl.create !Base.default_hashtbl_sz in
    let heapval_pp' = rec_print_pp !IConfig.print_depth visited heap in
    if !IConfig.show_exitval then Log.esl "exit value: %a" heapval_pp' retval

  let resolve_exitval (retval : Value.t) : Value.t =
    match retval with
    | Value.List [ Value.False; retval' ] -> retval'
    | Value.List [ Value.True; err ] ->
      Runtime_error.(throw (UncaughtExn (Value.str err)))
    | _ -> Log.fail "unexpected exit value: %a" Value.pp retval

  let result (heap : heap) (itool : ITooling.t ref) (v : Value.t) : IResult.t =
    let metrics = ITooling.Profiler.json !itool.pf in
    let retval = resolve_exitval v in
    (* FIXME: The show_exitval functionality should be on the interpret command *)
    show_exitval heap retval;
    { retval; heap; metrics }

  let eval_instrumented (entry : entry) (p : Prog.t) (inst : Instrument.t ref) :
    result =
    let fmain = get_func p entry.main no_region in
    let state = initial_state fmain entry.static_heap inst in
    Instrument.Tracer.trace_call !(state.inst).tr fmain (Stmt.Skip @> no_region);
    Instrument.Profiler.start !(state.inst).pf;
    let return = small_step_iter p state [ Func.body fmain ] in
    Instrument.Profiler.stop !(state.inst).pf state.heap;
    match return with
    | Final v -> result st.heap itool v
    | Error err -> Runtime_error.(throw (Failure (Value.str err)))
    | _ -> Log.fail "unexpected intermediate state"

  let set_interp_callbacks () : unit =
    let inst = ref (Instrument.initial_state ()) in
    let heapval_pp = heapval_pp !Config.print_depth in
    let state_conv (store, heap, stack) = { store; heap; stack; inst } in
    let eval_expr state = eval_expr @@ state_conv state in
    Tracer.set_interp_callbacks { heapval_pp };
    Debugger.set_interp_callbacks { heapval_pp; eval_expr }

  let eval_prog (entry : entry) (p : Prog.t) : result =
    let inst = ref (Instrument.initial_state ()) in
    set_interp_callbacks ();
    let execute () = eval_instrumented entry p inst in
    let finally () = Instrument.cleanup !inst in
    Fun.protect ~finally execute
end
