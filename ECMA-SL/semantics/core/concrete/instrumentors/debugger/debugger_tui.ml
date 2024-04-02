open Debugger_tui_helper
module Code = Debugger_tui_code
module View = Debugger_tui_view
module Execution = Debugger_tui_execution
module Terminal = Debugger_tui_terminal

type t =
  { acs : Acs.acs
  ; termwin : Win.t
  ; code : Code.t
  ; view : View.t
  ; exec : Execution.t
  ; term : Terminal.t
  }

let initialize () : t =
  try
    let w = initscr () in
    let (yz, xz) = get_size () in
    let termwin = Win.{ w; y = 0; x = 0; yz; xz } in
    let acs = get_acs_codes () in
    let code = Code.make acs termwin in
    let view = View.make acs termwin code.win.wframe in
    let exec = Execution.make acs termwin code.win.wframe in
    let term = Terminal.make termwin exec.win.wframe in
    !!(curs_set 0);
    !!(noecho ());
    !!(refresh ());
    { acs; termwin; code; view; exec; term }
  with exn -> endwin () |> fun () -> raise exn

let draw (tui : t) : unit =
  Code.draw tui.code;
  View.draw tui.view;
  Execution.draw tui.exec;
  Terminal.draw tui.term;
  !!(refresh ())

let terminate () : unit = endwin ()
