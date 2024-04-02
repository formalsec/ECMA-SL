open EslBase
open EslSyntax
module DebuggerTUI = Debugger_tui

module type M = sig
  type t

  val initial_state : unit -> t
  val cleanup : t -> unit
  val run : t -> Stmt.t -> t
end

module Disable : M = struct
  type t = unit

  let initial_state () : t = ()
  let cleanup (_ : t) : unit = ()
  let run (_ : t) (_ : Stmt.t) : t = ()
end

module Enable : M = struct
  type t' =
    { streams : Log.Redirect.t
    ; tui : DebuggerTUI.t
    }

  type t =
    | Initial
    | Step of t'
    | Final

  let initialize_debug_tui () : t' =
    let streams = Log.Redirect.capture Shared in
    try
      let tui = DebuggerTUI.initialize () in
      DebuggerTUI.draw_static tui;
      { streams; tui }
    with exn ->
      Log.Redirect.restore ~log:true streams;
      raise exn

  let terminate_debug_tui (db : t') : unit =
    DebuggerTUI.terminate ();
    Log.Redirect.restore ~log:true db.streams

  let run_debug_tui (s : Stmt.t) (db : t') : t =
    let tui = DebuggerTUI.data db.tui s in
    DebuggerTUI.render tui;
    ignore (Curses.getch ());
    Step { db with tui }

  let initial_state () : t = Initial

  let cleanup (db : t) : unit =
    match db with
    | Initial -> ()
    | Step db' -> terminate_debug_tui db'
    | Final -> ()

  let run (db : t) (s : Stmt.t) : t =
    match db with
    | Initial -> initialize_debug_tui () |> run_debug_tui s
    | Step tui -> run_debug_tui s tui
    | Final -> Final
end
