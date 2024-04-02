open Debugger_tui_helper

type t = { win : FrameWin.t }

let make (acs : Acs.acs) (termwin : Win.t) (codewin : Win.t) : t =
  let open FrameWin in
  let (y, x) = (0, codewin.xz - 1) in
  let yz = codewin.yz in
  let xz = termwin.xz - codewin.xz + 1 in
  let frame = { (default_frame acs) with tl = acs.ttee; bl = acs.btee } in
  let win = mk ~frame acs termwin y x yz xz in
  { win }

let draw (view : t) : unit =
  FrameWin.draw view.win;
  FrameWin.refresh view.win
