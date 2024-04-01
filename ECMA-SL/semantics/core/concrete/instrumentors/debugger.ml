open EslBase
module DebuggerTUI = Debugger_tui

module type M = sig
  type t

  val initial_state : unit -> t
  val cleanup : t -> unit
  val run : t -> t
end

module Disable : M = struct
  type t = unit

  let initial_state () : t = ()
  let cleanup (_ : t) : unit = ()
  let run (_ : t) : t = ()
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
    let tui = DebuggerTUI.initialize () in
    DebuggerTUI.draw tui;
    { streams; tui }

  let terminate_debug_tui (db : t') : unit =
    DebuggerTUI.terminate ();
    Log.Redirect.restore ~log:true db.streams

  let run_debug_tui (db : t') : t =
    ignore (Curses.getch ());
    Step db

  let initial_state () : t = Initial

  let cleanup (db : t) : unit =
    match db with
    | Initial -> ()
    | Step db' -> terminate_debug_tui db'
    | Final -> ()

  let run (db : t) : t =
    match db with
    | Initial -> initialize_debug_tui () |> run_debug_tui
    | Step tui -> run_debug_tui tui
    | Final -> Final
end
