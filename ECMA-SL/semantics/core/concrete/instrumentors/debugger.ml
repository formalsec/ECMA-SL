open Debugger_types
module DebuggerTUI = Debugger_tui
module InterpreterCallbacks = Debugger_cmd.InterpreterCallbacks

module BreakpointInjector = struct
  let rec inject_debug_innerscope (stack : stack) (cont : cont) : stack * cont =
    match cont with
    | { it; at } :: cont' when not at.real ->
      let (stack'', cont'') = inject_debug_innerscope stack cont' in
      (stack'', { it; at } :: cont'')
    | { it = Debug _; _ } :: _ -> (stack, cont)
    | { it = Block ss; at } :: cont' ->
      let (stack'', cont'') = inject_debug_innerscope stack ss in
      (stack'', { it = Block cont''; at } :: cont')
    | s :: cont' -> (stack, Source.(Stmt.Debug s @> s.at) :: cont')
    | [] -> inject_debug_outerscope stack cont

  and inject_debug_outerscope (stack : stack) (cont : cont) : stack * cont =
    let open Call_stack in
    match pop stack with
    | (Toplevel _, _) -> (stack, cont)
    | (Intermediate (loc, restore), stack') ->
      let (func, _) = Call_stack.location loc in
      let (store, cont', retvar) = Call_stack.restore restore in
      let (stack'', cont'') = inject_debug_innerscope stack' cont' in
      (push stack'' func store cont'' retvar, cont)
end

module type M = sig
  type t

  val initial_state : unit -> t
  val cleanup : t -> unit
  val set_interp_callbacks : InterpreterCallbacks.t -> unit
  val run : t -> state -> cont -> Stmt.t -> t * state * cont
  val call : t -> stack -> cont -> t * stack * cont
end

module Disable : M = struct
  type t = unit

  let initial_state () : t = ()
  let cleanup (_ : t) : unit = ()
  let set_interp_callbacks (_ : InterpreterCallbacks.t) : unit = ()

  let run (db : t) (st : state) (cont : cont) (_ : Stmt.t) : t * state * cont =
    (db, st, cont)

  let call (db : t) (stack : stack) (cont : cont) : t * stack * cont =
    (db, stack, cont)
end

module Enable : M = struct
  type t' =
    { streams : Log.Redirect.t
    ; tui : DebuggerTUI.t
    }

  type t =
    | Initial
    | StmtBreak of t'
    | CallBreak of t'
    | Final

  let initialize_debug_tui () : t' =
    let streams = Log.Redirect.capture Shared in
    try
      let tui = DebuggerTUI.initialize () in
      DebuggerTUI.render_static tui;
      { streams; tui }
    with exn ->
      Log.Redirect.restore ~log:true streams;
      raise exn

  let terminate_debug_tui (db : t') : unit =
    DebuggerTUI.terminate ();
    Log.Redirect.restore ~log:true db.streams

  let rec run_debug_tui_loop (tui : DebuggerTUI.t) : DebuggerTUI.t =
    let tui' = DebuggerTUI.update tui in
    if tui'.running then run_debug_tui_loop tui' else tui'

  let run_debug_tui (st : state) (s : Stmt.t) (db : t') : t' =
    let tui = DebuggerTUI.set_data db.tui st s in
    DebuggerTUI.render tui;
    DebuggerTUI.cursor tui;
    { db with tui = run_debug_tui_loop tui }

  let initial_state () : t = Initial

  let next_state (st : state) (cont : cont) (s : Stmt.t) (db : t') :
    t * state * cont =
    let open BreakpointInjector in
    let ( <@ ) inject_f (db, st, cont) =
      let (store, heap, stack) = st in
      let (stack', cont') = inject_f stack cont in
      (db, (store, heap, stack'), cont')
    in
    match (DebuggerTUI.get_last_cmd db.tui, s) with
    | (Step, _) -> inject_debug_innerscope <@ (StmtBreak db, st, cont)
    | (StepIn, { it = AssignCall _; _ }) -> (CallBreak db, st, cont)
    | (StepIn, _) -> inject_debug_innerscope <@ (StmtBreak db, st, cont)
    | (StepOut, _) -> inject_debug_outerscope <@ (StmtBreak db, st, cont)
    | (Continue, _) -> (StmtBreak db, st, cont)
    | (Exit, _) -> terminate_debug_tui db |> fun () -> (Final, st, cont)
    | _ -> Internal_error.(throw __FUNCTION__ (Expecting "debugger flow action"))

  let cleanup (db : t) : unit =
    match db with
    | Initial -> ()
    | StmtBreak db' -> terminate_debug_tui db'
    | CallBreak db' -> terminate_debug_tui db'
    | Final -> ()

  let set_interp_callbacks (interp_callbacks : InterpreterCallbacks.t) : unit =
    InterpreterCallbacks.val_pp := interp_callbacks.val_pp;
    InterpreterCallbacks.eval_expr := interp_callbacks.eval_expr

  let run (db : t) (st : state) (cont : cont) (s : Stmt.t) : t * state * cont =
    let run_debug_tui' = run_debug_tui st s in
    let next_state' = next_state st cont s in
    match db with
    | Initial -> initialize_debug_tui () |> run_debug_tui' |> next_state'
    | StmtBreak tui -> run_debug_tui' tui |> next_state'
    | CallBreak tui -> run_debug_tui' tui |> next_state'
    | Final -> (Final, st, cont)

  let call (db : t) (stack : stack) (cont : cont) : t * stack * cont =
    let open BreakpointInjector in
    match db with
    | CallBreak db' ->
      let (stack', cont') = inject_debug_innerscope stack cont in
      (StmtBreak db', stack', cont')
    | _ -> (db, stack, cont)
end
