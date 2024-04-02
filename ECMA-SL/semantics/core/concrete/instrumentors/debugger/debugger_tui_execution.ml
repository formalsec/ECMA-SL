open Debugger_tui_helper

type t = { win : FrameWin.t }

let make (acs : Acs.acs) (termwin : Win.t) (codewin : Win.t) : t =
  let open FrameWin in
  let (y, x) = (codewin.yz - 1, 0) in
  let yz = termwin.yz - codewin.yz + 1 in
  let xz = proportional_sz termwin.xz 5 2 in
  let frame = { (default_frame acs) with tl = acs.ltee; tr = acs.ttee } in
  let win = mk ~frame acs termwin y x yz xz in
  { win }

let draw (exec : t) : unit =
  FrameWin.draw exec.win;
  FrameWin.refresh exec.win
