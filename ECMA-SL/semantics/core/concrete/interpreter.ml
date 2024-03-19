open EslBase
open EslSyntax
open EslSyntax.Source

module EntryPoint = struct
  type t =
    { main : string
    ; resolve_exitval : bool
    ; static_heap : Val.t Heap.t option
    }

  let default : t =
    { main = "main"; resolve_exitval = true; static_heap = None }
end

module M (Instrument : Instrument.M) = struct
  type obj = Val.t Object.t
  type store = Val.t Store.t
  type heap = Val.t Heap.t
  type stack = store Call_stack.t

  type state =
    { lvl : int
    ; store : store
    ; heap : heap
    ; stack : stack
    }

  type return =
    | Final of Val.t
    | Error of Val.t
    | Intermediate of state * Stmt.t list

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

  let set_global_var (store : store) (heap : heap) : unit =
    let open Compiler.Const in
    if Option.is_some (Heap.get_opt heap esl_globals_loc) then
      Store.set store esl_globals_obj (Loc esl_globals_loc)

  let initial_state (fmain : Func.t) (s_heap : heap option) : state =
    let store = Store.create [] in
    let heap = Option.fold ~none:(Heap.create ()) ~some:Heap.extend s_heap in
    let stack = Call_stack.create fmain in
    set_global_var store heap;
    { lvl = 0; store; heap; stack }

  let operate (eval_op_fun : unit -> Val.t) (es : Expr.t list) : Val.t =
    try eval_op_fun ()
    with Runtime_error.Error err ->
      let e = Runtime_error.(src err |> ErrSrc.index_to_el es) in
      Runtime_error.(set_src (ErrSrc.at e) err |> raise)

  let print_val (heap : heap) (v : Val.t) : unit =
    let open Fmt in
    match v with
    | Str s -> printf "%s@." s
    | Loc l -> printf "%a@." (Object.pp Val.pp) (get_loc heap l)
    | _ -> printf "%a@." Val.pp v

  let rec eval_expr' (state : state) (e : Expr.t) : Val.t =
    match e.it with
    | Val v -> v
    | Var x -> get_var state.store x e.at
    | UnOpt (op, e') ->
      let v = eval_expr state e' in
      let eval_op_fun () = Eval_operator.eval_unopt op v in
      operate eval_op_fun [ e' ]
    | BinOpt (op, e1, e2) ->
      let v1 = eval_expr state e1 in
      let v2 = eval_expr state e2 in
      let eval_op_fun () = Eval_operator.eval_binopt op v1 v2 in
      operate eval_op_fun [ e1; e2 ]
    | TriOpt (op, e1, e2, e3) ->
      let v1 = eval_expr state e1 in
      let v2 = eval_expr state e2 in
      let v3 = eval_expr state e3 in
      let eval_op_fun () = Eval_operator.eval_triopt op v1 v2 v3 in
      operate eval_op_fun [ e1; e2; e3 ]
    | NOpt (op, es) ->
      let vs = List.map (eval_expr state) es in
      let eval_op_fun () = Eval_operator.eval_nopt op vs in
      operate eval_op_fun es
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
        Internal_error.(throw __FUNCTION__ (NotImplemented (Some "symbolic"))) )

  and eval_expr (state : state) (e : Expr.t) : Val.t =
    let v = eval_expr' state e in
    Instrument.Tracer.trace_expr state.lvl e (state.heap, v);
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

  let update_mon_label (inst : Instrument.t) (s : Stmt.t)
    (s_eval : Monitor.stmt_eval) : Instrument.t =
    { inst with mon_label = Instrument.Monitor.generate_label s s_eval }

  let eval_small_step (p : Prog.t) (state : state) (inst : Instrument.t)
    (s : Stmt.t) (cont : Stmt.t list) : return * Instrument.t =
    let lbl inst s_eval = update_mon_label inst s s_eval in
    Call_stack.update state.stack s;
    Instrument.Tracer.trace_stmt state.lvl s;
    match s.it with
    | Skip -> (Intermediate (state, cont), lbl inst SkipEval)
    | Merge -> (Intermediate (state, cont), lbl inst MergeEval)
    | Debug s ->
      let args = (state.store, state.heap, state.stack, inst.db) in
      let (db', stack', cont') = Instrument.Debugger.run args s cont in
      let state' = { state with stack = stack' } in
      let inst' = { inst with db = db' } in
      (Intermediate (state', cont'), lbl inst' DebugEval)
    | Block ss -> (Intermediate (state, ss @ cont), lbl inst BlockEval)
    | Print e ->
      eval_expr state e |> print_val state.heap;
      (Intermediate (state, cont), lbl inst PrintEval)
    | Return e -> (
      let v = eval_expr state e in
      let f = Call_stack.func state.stack in
      Instrument.Tracer.trace_return state.lvl f s (state.heap, v);
      let (frame, stack') = Call_stack.pop state.stack in
      match frame with
      | Call_stack.Toplevel _ -> (Final v, lbl inst ReturnEval)
      | Call_stack.Intermediate (_, restore) ->
        let (store', cont', x) = Call_stack.restore restore in
        let lvl = state.lvl - 1 in
        let state' = { state with lvl; store = store'; stack = stack' } in
        Store.set store' x v;
        Instrument.Tracer.trace_restore lvl (Call_stack.func stack');
        (Intermediate (state', cont'), lbl inst ReturnEval) )
    | Assign (x, e) ->
      eval_expr state e |> Store.set state.store x.it;
      (Intermediate (state, cont), lbl inst AssignEval)
    | AssignCall (x, fe, es) -> (
      let (fn, fvs) = eval_func_expr state fe in
      let vs = fvs @ List.map (eval_expr state) es in
      match Instrument.Monitor.interceptor fn vs es with
      | Some lbl -> (Intermediate (state, cont), { inst with mon_label = lbl })
      | None ->
        let f = get_func p fn fe.at in
        let (stack, store) = (state.stack, state.store) in
        let (stack', store') = prepare_call stack f store cont x.it vs fe.at in
        let cont' = [ Func.body f ] in
        let inject_res = Instrument.Debugger.inject s inst.db stack' cont' in
        let (db', stack'', cont'') = inject_res in
        let lvl = state.lvl + 1 in
        let state' = { state with lvl; store = store'; stack = stack'' } in
        let inst' = { inst with db = db' } in
        Instrument.Tracer.trace_call lvl f s;
        (Intermediate (state', cont''), lbl inst' (AssignCallEval f)) )
    | AssignECall (x, fn, es) ->
      let vs = List.map (eval_expr state) es in
      let v = External.execute p state.store state.heap fn.it vs in
      Store.set state.store x.it v;
      (Intermediate (state, cont), lbl inst AssignECallEval)
    | AssignNewObj x ->
      let l = Loc.create () in
      Heap.set state.heap l (Object.create ());
      Store.set state.store x.it (Val.Loc l);
      (Intermediate (state, cont), lbl inst (AssignNewObjEval l))
    | AssignObjToList (x, e) ->
      let fld_to_tup_f (fn, fv) = Val.Tuple [ Str fn; fv ] in
      let (_, obj) = eval_object state state.heap e in
      let v = Val.List (Object.fld_lst obj |> List.map fld_to_tup_f) in
      Store.set state.store x.it v;
      (Intermediate (state, cont), lbl inst AssignObjToListEval)
    | AssignObjFields (x, e) ->
      let fld_to_tup_f (fn, _) = Val.Str fn in
      let (_, obj) = eval_object state state.heap e in
      let v = Val.List (Object.fld_lst obj |> List.map fld_to_tup_f) in
      Store.set state.store x.it v;
      (Intermediate (state, cont), lbl inst AssignObjFieldsEval)
    | AssignInObjCheck (x, fe, oe) ->
      let in_obj = function Some _ -> true | None -> false in
      let (loc, obj) = eval_object state state.heap oe in
      let fn = eval_string state fe in
      let v = Val.Bool (Object.get obj fn |> in_obj) in
      Store.set state.store x.it v;
      (Intermediate (state, cont), lbl inst (AssignInObjCheckEval (loc, fn)))
    | FieldLookup (x, oe, fe) ->
      let fld_val v = Option.value ~default:(Val.Symbol "undefined") v in
      let (l, obj) = eval_object state state.heap oe in
      let fn = eval_string state fe in
      let v = Object.get obj fn |> fld_val in
      Store.set state.store x.it v;
      (Intermediate (state, cont), lbl inst (FieldLookupEval (l, fn)))
    | FieldAssign (oe, fe, e) ->
      let (l, obj) = eval_object state state.heap oe in
      let fn = eval_string state fe in
      let v = eval_expr state e in
      Object.set obj fn v;
      (Intermediate (state, cont), lbl inst (FieldAssignEval (l, fn)))
    | FieldDelete (oe, fe) ->
      let (l, obj) = eval_object state state.heap oe in
      let fn = eval_string state fe in
      Object.delete obj fn;
      (Intermediate (state, cont), lbl inst (FieldDeleteEval (l, fn)))
    | If (e, s1, s2) -> (
      let v = eval_boolean state e in
      let s2' = Option.value ~default:(Stmt.Skip @> no_region) s2 in
      match (v, s1.it, s2'.it) with
      | (true, Block ss, _) ->
        let cont' = ss @ ((Stmt.Merge @?> s1.at) :: cont) in
        (Intermediate (state, cont'), lbl inst (IfEval true))
      | (false, _, Block ss) ->
        let cont' = ss @ ((Stmt.Merge @?> s2'.at) :: cont) in
        (Intermediate (state, cont'), lbl inst (IfEval false))
      | (false, _, Skip) -> (Intermediate (state, cont), lbl inst (IfEval false))
      | (true, _, _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "if block"))
      | (false, _, _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "else block")) )
    | While (e, s') ->
      let loop = Stmt.If (e, Stmt.Block [ s'; s ] @?> s'.at, None) @?> s.at in
      (Intermediate (state, loop :: cont), lbl inst WhileEval)
    | Switch (e, css, dflt) -> (
      let v = eval_expr state e in
      match (Hashtbl.find_opt css v, dflt) with
      | (Some { it = Block ss; at }, _) | (None, Some { it = Block ss; at }) ->
        let cont' = ss @ ((Stmt.Merge @?> at) :: cont) in
        (Intermediate (state, cont'), lbl inst (SwitchEval v))
      | (Some _, _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "switch block"))
      | (None, Some _) ->
        Internal_error.(throw __FUNCTION__ (Expecting "sdflt block"))
      | (None, None) -> (Intermediate (state, cont), lbl inst (SwitchEval v)) )
    | Fail e ->
      let v = eval_expr state e in
      (Error v, lbl inst FailEval)
    | Assert e ->
      let v = eval_boolean state e in
      if v then (Intermediate (state, cont), lbl inst (AssertEval true))
      else
        let err = Fmt.asprintf "Assert false: %a" Expr.pp e in
        (Error (Val.Str err), lbl inst (AssertEval false))

  let eval_small_step_safe (p : Prog.t) (state : state) (inst : Instrument.t)
    (s : Stmt.t) (cont : Stmt.t list) : return * Instrument.t =
    try eval_small_step p state inst s cont
    with Runtime_error.Error err ->
      Runtime_error.(set_trace state.stack err |> raise)

  let rec small_step_iter (p : Prog.t) (state : state) (inst : Instrument.t)
    (ss : Stmt.t list) : return =
    match ss with
    | [] ->
      let fn = Func.name (Call_stack.func state.stack) in
      Runtime_error.(throw ~src:(ErrSrc.at fn) (MissingReturn fn))
    | s :: cont -> (
      let (return, inst') = eval_small_step_safe p state inst s cont in
      let module Mon = Instrument.Monitor in
      let mon_return = Mon.eval_small_step inst.mon_state inst.mon_label in
      let mon_state' = Mon.next_state mon_return in
      let inst'' = { inst' with mon_state = mon_state' } in
      match return with
      | Final v -> Final v
      | Error v -> Error v
      | Intermediate (state', cont') -> small_step_iter p state' inst'' cont' )

  let resolve_exitval (retval : Val.t) : Val.t =
    match retval with
    | Val.Tuple [ Val.Bool false; retval' ] -> retval'
    | Val.Tuple [ Val.Bool true; err ] ->
      Runtime_error.(throw (UncaughtExn (Val.str err)))
    | _ -> Runtime_error.(throw (UnexpectedExitVal retval))

  let eval_partial (entry : EntryPoint.t) (p : Prog.t) : Val.t * heap =
    let fmain = get_func p entry.main no_region in
    let state = initial_state fmain entry.static_heap in
    let inst = Instrument.intial_state () in
    Instrument.Tracer.trace_restore (-1) fmain;
    let return = small_step_iter p state inst [ Func.body fmain ] in
    match return with
    | Final v when entry.resolve_exitval -> (resolve_exitval v, state.heap)
    | Final v -> (v, state.heap)
    | Error err -> Runtime_error.(throw (Failure (Val.str err)))
    | _ -> Internal_error.(throw __FUNCTION__ (Expecting "final/error state"))

  let eval_prog ?(entry : EntryPoint.t = EntryPoint.default) (p : Prog.t) :
    Val.t =
    fst (eval_partial entry p)
end
