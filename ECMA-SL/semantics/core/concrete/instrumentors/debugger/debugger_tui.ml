open Debugger_tui_helper

module Code = struct
  type t = { win : FrameWin.t }

  let make (acs : Acs.acs) (termwin : Win.t) : t =
    let (x, y) = (0, 0) in
    let xz = proportional_sz termwin.xz 3 2 in
    let yz = proportional_sz termwin.yz 5 3 in
    let win = FrameWin.mk acs termwin x y xz yz in
    { win }

  let draw (code : t) : unit =
    FrameWin.draw code.win;
    FrameWin.refresh code.win
end

module View = struct
  type t = { win : FrameWin.t }

  let make (acs : Acs.acs) (termwin : Win.t) (codewin : Win.t) : t =
    let open FrameWin in
    let (x, y) = (codewin.xz - 1, 0) in
    let xz = termwin.xz - codewin.xz + 1 in
    let yz = codewin.yz in
    let frame = { (default_frame acs) with tl = acs.ttee; bl = acs.btee } in
    let win = mk ~frame acs termwin x y xz yz in
    { win }

  let draw (view : t) : unit =
    FrameWin.draw view.win;
    FrameWin.refresh view.win
end

module Execution = struct
  type t = { win : FrameWin.t }

  let make (acs : Acs.acs) (termwin : Win.t) (codewin : Win.t) : t =
    let open FrameWin in
    let (x, y) = (0, codewin.yz - 1) in
    let xz = proportional_sz termwin.xz 5 2 in
    let yz = termwin.yz - codewin.yz + 1 in
    let frame = { (default_frame acs) with tl = acs.ltee; tr = acs.ttee } in
    let win = mk ~frame acs termwin x y xz yz in
    { win }

  let draw (exec : t) : unit =
    FrameWin.draw exec.win;
    FrameWin.refresh exec.win
end

module Terminal = struct
  type t = { win : Win.t }

  let make (termwin : Win.t) (execwin : Win.t) : t =
    let (x, y) = (execwin.xz, execwin.xz + 1) in
    let xz = termwin.xz - execwin.xz in
    let yz = termwin.yz - execwin.yz - 1 in
    let win = Win.mk termwin x y xz yz in
    { win }

  let draw (_term : t) : unit = ()
end

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
    let termwin = Win.{ w; x = 0; y = 0; xz; yz } in
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
