open EslBase
open EslSyntax
open EslSyntax.Source

type entry =
  { main : string
  ; static_heap : Value.t Heap.t option
  }

let entry_default () : entry = { main = "main"; static_heap = None }

type result =
  { retval : Value.t
  ; heap : Value.t Heap.t
  ; metrics : Yojson.Basic.t
  }

module M (Instrument : Instrument.M) = struct
  module E = Smtml.Expr

  type obj = Value.t Object.t
  type store = Value.t Store.t
  type heap = Value.t Heap.t
  type stack = store Call_stack.t

  type state =
    { store : store
    ; heap : heap
    ; stack : stack
    ; inst : Instrument.t ref
    }

  type return =
    | Final of Value.t
    | Error of Value.t
    | Intermediate of state * Stmt.t list

  module Config = struct
    let print_depth : int option ref = ref None
    let resolve_exitval : bool ref = ref true
    let show_exitval : bool ref = ref false
  end

  let set_global_var (store : store) (heap : heap) : unit =
    let open Compiler.Const in
    if Option.is_some (Heap.get_opt heap esl_globals_loc) then
      Store.set store esl_globals_obj (App (`Op "loc", [ Int esl_globals_loc ]))

  let initial_state (fmain : Func.t) (s_heap : heap option)
    (inst : Instrument.t ref) : state =
    let store = Store.create [] in
    let heap = Option.fold ~none:(Heap.create ()) ~some:Heap.extend s_heap in
    let stack = Call_stack.create fmain in
    set_global_var store heap;
    { store; heap; stack; inst }

  let get_var (store : store) (x : string) (at : at) : Value.t =
    match Store.get_opt store x with
    | None -> Runtime_error.(throw ~src:(ErrSrc.at at) (UnknownVar x))
    | Some v -> v

  let get_loc (heap : heap) (l : Loc.t) : obj =
    match Heap.get_opt heap l with
    | None -> Log.fail "expecting existing location"
    | Some obj -> obj

  let get_func (p : Prog.t) (fn : string) (at : at) : Func.t =
    match Prog.func_opt p fn with
    | None -> Runtime_error.(throw ~src:(ErrSrc.at at) (UnknownFunc fn))
    | Some f -> f

  let eval_op (op_lbl_f : unit -> string) (eval_op_f : unit -> Value.t) :
    Value.t =
    try eval_op_f () with
    | Runtime_error.Error err ->
      Runtime_error.(push (OpEvalErr (op_lbl_f ())) err |> raise)
    | err -> Log.fail "unexpected operator error: %s" (Printexc.to_string err)

  let rec eval_expr' (state : state) (expr : Expr.t) : Value.t =
    match expr.it with
    | Val v -> v
    | Var x -> get_var state.store x expr.at
    | UnOpt (op, e) ->
      let arg = (eval_expr state e, e.at) in
      let op_lbl_f () = Operator.unopt_label op in
      let op_eval_f () = Eval_op.unopt_semantics op arg in
      eval_op op_lbl_f op_eval_f
    | BinOpt (op, e1, e2) ->
      let arg1 = (eval_expr state e1, e1.at) in
      let arg2 = (eval_expr state e2, e2.at) in
      let op_lbl_f () = Operator.binopt_label op in
      let op_eval_f () = Eval_op.binopt_semantics op (arg1, arg2) in
      eval_op op_lbl_f op_eval_f
    | TriOpt (op, e1, e2, e3) ->
      let arg1 = (eval_expr state e1, e1.at) in
      let arg2 = (eval_expr state e2, e2.at) in
      let arg3 = (eval_expr state e3, e3.at) in
      let op_lbl_f () = Operator.triopt_label op in
      let op_eval_f () = Eval_op.triopt_semantics op (arg1, arg2, arg3) in
      eval_op op_lbl_f op_eval_f
    | NOpt (op, es) ->
      let args = List.map (fun e -> (eval_expr state e, e.at)) es in
      let op_lbl_f () = Operator.nopt_label op in
      let op_eval_f () = Eval_op.nopt_semantics op args in
      eval_op op_lbl_f op_eval_f
    | Curry (fe, es) -> (
      let fv = eval_expr state fe in
      let vs = List.map (eval_expr state) es in
      match fv with
      | Str fn -> Value.App (`Op fn, vs)
      | _ -> Runtime_error.(throw ~src:(ErrSrc.from fe) (BadExpr ("curry", fv)))
      )

  and eval_expr (state : state) (e : Expr.t) : Value.t =
    let v = eval_expr' state e in
    let lvl = Call_stack.level state.stack in
    Instrument.Profiler.count !(state.inst).pf `Expr;
    Instrument.Tracer.trace_expr lvl e (state.heap, v);
    v

  let eval_str (state : state) (e : Expr.t) : string =
    match eval_expr state e with
    | Str s -> s
    | _ as v -> Runtime_error.(throw ~src:(ErrSrc.from e) (BadVal ("string", v)))

  let eval_bool (state : state) (e : Expr.t) : bool =
    match eval_expr state e with
    | Value.True -> true
    | Value.False -> false
    | _ as v ->
      Runtime_error.(throw ~src:(ErrSrc.from e) (BadVal ("boolean", v)))

  let eval_loc (state : state) (e : Expr.t) : Loc.t =
    match eval_expr state e with
    | App (`Op "loc", [ Int l ]) -> l
    | _ as v ->
      Runtime_error.(throw ~src:(ErrSrc.from e) (BadVal ("location", v)))

  let eval_obj (state : state) (heap : heap) (e : Expr.t) : Loc.t * obj =
    let l = eval_loc state e in
    let obj = get_loc heap l in
    (l, obj)

  let eval_func_expr (state : state) (fe : Expr.t) : string * Value.t list =
    match eval_expr state fe with
    | Value.Str fn -> (fn, [])
    | Value.App (`Op fn, fvs) -> (fn, fvs)
    | _ as v -> Runtime_error.(throw ~src:(ErrSrc.from fe) (BadFuncId v))

  let rec heapval_pp (depth : int option) (visited : (Loc.t, unit) Hashtbl.t)
    (heap : heap) (ppf : Fmt.t) (v : Value.t) : unit =
    let valid_depth = Option.fold ~none:true ~some:(fun d -> d > 0) depth in
    let visited_loc = Hashtbl.mem visited in
    let incr_depth = Option.map (fun d -> d - 1) in
    let heapval_pp' = heapval_pp (incr_depth depth) visited heap in
    match v with
    | App (`Op "loc", [ Int l ]) when (not valid_depth) || visited_loc l ->
      Fmt.fmt ppf "{...}"
    (* | Arr _ when not valid_depth -> Fmt.fmt ppf "[|...|]" *)
    | List _ when not valid_depth -> Fmt.fmt ppf "[...]"
    (* | Tuple _ when not valid_depth -> Fmt.fmt ppf "(...)" *)
    | App (`Op "loc", [ Int l ]) ->
      Hashtbl.add visited l ();
      (Object.pp heapval_pp') ppf (get_loc heap l);
      Hashtbl.remove visited l
    | _ -> Value.pp_custom_val heapval_pp' ppf v

  let print_pp (heap : heap) (ppf : Fmt.t) (v : Value.t) : unit =
    match v with
    | Str s -> Fmt.pp_str ppf s
    | _ ->
      let visited = Hashtbl.create !Base.default_hashtbl_sz in
      heapval_pp !Config.print_depth visited heap ppf v

  let prepare_store_binds (pxs : string list) (vs : Value.t list) (at : at) :
    (string * Value.t) list =
    try List.combine pxs vs
    with Invalid_argument _ ->
      let (xpxs, nargs) = (List.length pxs, List.length vs) in
      Runtime_error.(throw ~src:(ErrSrc.at at) (BadNArgs (xpxs, nargs)))

  let prepare_call (stack : stack) (f : Func.t) (store : store)
    (cont : Stmt.t list) (x : string) (vs : Value.t list) (at : at) :
    stack * store =
    let pxs = Func.params' f in
    let stack' = Call_stack.push stack f store cont x in
    let store' = Store.create (prepare_store_binds pxs vs at) in
    (stack', store')

  let eval_small_step (p : Prog.t) (state : state) (s : Stmt.t)
    (cont : Stmt.t list) : return =
    let inst = !(state.inst) in
    let lbl_f = Instrument.Monitor.update_label in
    let ( $$ ) v s_eval = lbl_f inst.mon s s_eval |> fun () -> v in
    let lvl = Call_stack.level state.stack in
    Instrument.Tracer.trace_stmt lvl s;
    Instrument.Profiler.count inst.pf `Stmt;
    match s.it with
    | Skip -> Intermediate (state, cont) $$ SkipEval
    | Merge -> Intermediate (state, cont) $$ MergeEval
    | Debug s' ->
      let db_state = (state.store, state.heap, state.stack) in
      let db_res = Instrument.Debugger.run inst.db db_state cont s' in
      let ((store, heap, stack), cont') = db_res in
      Intermediate ({ state with store; heap; stack }, s' :: cont') $$ DebugEval
    | Block ss -> Intermediate (state, ss @ cont) $$ BlockEval
    | Print e ->
      Log.stdout "%a@." (print_pp state.heap) (eval_expr state e);
      Intermediate (state, cont) $$ PrintEval
    | Return e -> (
      let v = eval_expr state e in
      let f = Call_stack.func state.stack in
      Instrument.Tracer.trace_return lvl f s (state.heap, v);
      let (frame, stack') = Call_stack.pop state.stack in
      match frame with
      | Call_stack.Toplevel _ -> Final v $$ ReturnEval
      | Call_stack.Intermediate (_, restore) ->
        let (store', cont', x) = Call_stack.restore restore in
        let state' = { state with store = store'; stack = stack' } in
        Store.set store' x v;
        Instrument.Tracer.trace_restore (lvl - 1) (Call_stack.func stack');
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
        Instrument.Tracer.trace_call (lvl + 1) f s;
        Intermediate (state', cont'') $$ AssignCallEval f )
    | AssignECall (x, fn, es) ->
      let vs = List.map (eval_expr state) es in
      let v = External.execute p state.store state.heap fn.it vs in
      Store.set state.store x.it v;
      Intermediate (state, cont) $$ AssignECallEval
    | AssignNewObj x ->
      let l = Loc.create () in
      Heap.set state.heap l (Object.create ());
      Store.set state.store x.it (App (`Op "loc", [ Int l ]));
      Intermediate (state, cont) $$ AssignNewObjEval l
    | AssignObjToList (x, e) ->
      let fld_to_tup_f (fn, fv) = Value.List [ Str fn; fv ] in
      let (_, obj) = eval_obj state state.heap e in
      let v = Value.List (Object.fld_lst obj |> List.map fld_to_tup_f) in
      Store.set state.store x.it v;
      Intermediate (state, cont) $$ AssignObjToListEval
    | AssignObjFields (x, e) ->
      let fld_to_tup_f (fn, _) = Value.Str fn in
      let (_, obj) = eval_obj state state.heap e in
      let v = Value.List (Object.fld_lst obj |> List.map fld_to_tup_f) in
      Store.set state.store x.it v;
      Intermediate (state, cont) $$ AssignObjFieldsEval
    | AssignInObjCheck (x, fe, oe) ->
      let in_obj = function Some _ -> true | None -> false in
      let (loc, obj) = eval_obj state state.heap oe in
      let fn = eval_str state fe in
      let v = if Object.get obj fn |> in_obj then Value.True else Value.False in
      Store.set state.store x.it v;
      Intermediate (state, cont) $$ AssignInObjCheckEval (loc, fn)
    | FieldLookup (x, oe, fe) ->
      let fld_val v =
        Option.value ~default:(Value.App (`Op "symbol", [ Str "undefined" ])) v
      in
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
      let s2' = Option.value ~default:(Stmt.Skip @> none) s2 in
      match (v, s1.it, s2'.it) with
      | (true, Block ss, _) ->
        let cont' = ss @ ((Stmt.Merge @?> s1.at) :: cont) in
        Intermediate (state, cont') $$ IfEval true
      | (false, _, Block ss) ->
        let cont' = ss @ ((Stmt.Merge @?> s2'.at) :: cont) in
        Intermediate (state, cont') $$ IfEval false
      | (false, _, Skip) -> Intermediate (state, cont) $$ IfEval false
      | (true, _, _) -> Log.fail "expecting if block"
      | (false, _, _) -> Log.fail "expecting else block" )
    | While (e, s') ->
      let loop = Stmt.If (e, Stmt.Block [ s'; s ] @?> s'.at, None) @?> s.at in
      Intermediate (state, loop :: cont) $$ WhileEval
    | Switch (e, css, dflt) -> (
      let v = eval_expr state e in
      match (Hashtbl.find_opt css v, dflt) with
      | (Some { it = Block ss; at }, _) | (None, Some { it = Block ss; at }) ->
        let cont' = ss @ ((Stmt.Merge @?> at) :: cont) in
        Intermediate (state, cont') $$ SwitchEval v
      | (Some _, _) -> Log.fail "expecting switch block"
      | (None, Some _) -> Log.fail "expecting sdflt block"
      | (None, None) -> Intermediate (state, cont) $$ SwitchEval v )
    | Fail e ->
      let v = eval_expr state e in
      Error v $$ FailEval
    | Assert e ->
      let v = eval_bool state e in
      if v then Intermediate (state, cont) $$ AssertEval true
      else
        let err = Fmt.str "Assert false: %a" Expr.pp e in
        Error (Value.Str err) $$ AssertEval false

  let eval_small_step_safe (p : Prog.t) (state : state) (s : Stmt.t)
    (cont : Stmt.t list) : return =
    let state' = { state with stack = Call_stack.update state.stack s } in
    try eval_small_step p state' s cont
    with Runtime_error.Error err ->
      Runtime_error.(set_trace state.stack err |> raise)

  let rec small_step_iter (p : Prog.t) (state : state) (ss : Stmt.t list) :
    return =
    match ss with
    | [] ->
      let fn = Func.name (Call_stack.func state.stack) in
      Runtime_error.(throw ~src:(ErrSrc.from fn) (MissingReturn fn))
    | s :: cont -> (
      let return = eval_small_step_safe p state s cont in
      Instrument.Monitor.eval_small_step !(state.inst).mon;
      match return with
      | Final v -> Final v
      | Error v -> Error v
      | Intermediate (state', cont') -> small_step_iter p state' cont' )

  let debugger_interp_callbacks () : unit =
    let inst = ref (Instrument.initial_state ()) in
    let heapval_pp = heapval_pp !Config.print_depth in
    let state_conv (store, heap, stack) = { store; heap; stack; inst } in
    let eval_expr state = eval_expr @@ state_conv state in
    Instrument.Debugger.set_interp_callbacks { heapval_pp; eval_expr }

  let resolve_exitval (retval : Value.t) : Value.t =
    if not !Config.resolve_exitval then retval
    else
      match retval with
      | Value.List [ Value.False; retval' ] -> retval'
      | Value.List [ Value.True; err ] ->
        Runtime_error.(throw (UncaughtExn (Value.to_string err)))
      | _ -> Runtime_error.(throw (UnexpectedExitVal retval))

  let show_exitval (heap : heap) (retval : Value.t) : unit =
    let visited = Hashtbl.create !Base.default_hashtbl_sz in
    let heapval_pp' = heapval_pp !Config.print_depth visited heap in
    if !Config.show_exitval then Log.esl "exit value: %a" heapval_pp' retval

  let result (v : Value.t) (heap : heap) (inst : Instrument.t ref) : result =
    let metrics = Instrument.Profiler.json !inst.pf in
    let retval = resolve_exitval v in
    show_exitval heap retval;
    { retval; heap; metrics }

  let eval_instrumented (entry : entry) (p : Prog.t) (inst : Instrument.t ref) :
    result =
    let fmain = get_func p entry.main none in
    let state = initial_state fmain entry.static_heap inst in
    Instrument.Tracer.trace_restore (-1) fmain;
    Instrument.Profiler.start !(state.inst).pf;
    let return = small_step_iter p state [ Func.body fmain ] in
    Instrument.Profiler.stop !(state.inst).pf state.heap;
    match return with
    | Final v -> result v state.heap inst
    | Error err -> Runtime_error.(throw (Failure (Value.to_string err)))
    | _ -> Log.fail "unexpected intermediate state"

  let eval_prog (entry : entry) (p : Prog.t) : result =
    let inst = ref (Instrument.initial_state ()) in
    let execute () = eval_instrumented entry p inst in
    let finally () = Instrument.cleanup !inst in
    Fun.protect ~finally execute
end
