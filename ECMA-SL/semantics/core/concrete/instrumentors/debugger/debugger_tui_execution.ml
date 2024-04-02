open Debugger_tui_helper

type t = { ui : Win.t Frame.t }

let create (acs : Acs.acs) (termwin : Win.t) (codewin : Win.t) : t =
  let (y, x) = (codewin.yz - 1, 0) in
  let yz = termwin.yz - codewin.yz + 1 in
  let xz = proportional_sz termwin.xz 5 2 in
  let frame = Frame.{ (default_frame acs) with tl = acs.ltee; tr = acs.ttee } in
  let win = Win.mk termwin 1 1 (yz - 2) (xz - 2) in
  let ui = Frame.mk ~frame acs termwin y x yz xz (Win.element win) in
  { ui }

let draw_static (exec : t) : unit =
  Frame.draw exec.ui;
  Frame.refresh exec.ui
