open Debugger_tui_helper

type t = { win : FrameWin.t }

let make (acs : Acs.acs) (termwin : Win.t) : t =
  let (y, x) = (0, 0) in
  let yz = proportional_sz termwin.yz 5 3 in
  let xz = proportional_sz termwin.xz 3 2 in
  let win = FrameWin.mk acs termwin y x yz xz in
  { win }

let draw (code : t) : unit =
  FrameWin.draw code.win;
  FrameWin.refresh code.win
