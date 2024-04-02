open Debugger_tui_helper
open EslSyntax
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

let test_term_size (yz : int) (xz : int) : unit =
  let open EslBase in
  if yz < Terminal.Config.min_height || xz < Terminal.Config.min_width then (
    endwin ();
    Internal_error.(throw __FUNCTION__ (Expecting "larger terminal size")) )

let initialize () : t =
  try
    let w = initscr () in
    let (yz, xz) = get_size () in
    test_term_size yz xz;
    let termwin = Win.{ w; y = 0; x = 0; yz; xz } in
    let acs = get_acs_codes () in
    let code = Code.create acs termwin in
    let view = View.create acs termwin code.ui.wframe in
    let exec = Execution.create acs termwin code.ui.wframe in
    let term = Terminal.create termwin exec.ui.wframe in
    !!(curs_set 0);
    !!(noecho ());
    !!(start_color ());
    !!(use_default_colors ());
    { acs; termwin; code; view; exec; term }
  with exn -> endwin () |> fun () -> raise exn

let data (tui : t) (s : Stmt.t) : t =
  let code = Code.data tui.code s in
  { tui with code }

let draw_static (tui : t) : unit =
  Code.draw_static tui.code;
  View.draw_static tui.view;
  Execution.draw_static tui.exec;
  Terminal.draw_static tui.term

let render (tui : t) : unit = Code.render tui.code
let terminate () : unit = endwin ()
