open Debugger_tui_helper

type t = { ui : Win.t Frame.t }

let create (acs : Acs.acs) (termwin : Win.t) (codewin : Win.t) : t =
  let (y, x) = (0, codewin.xz - 1) in
  let yz = codewin.yz in
  let xz = termwin.xz - codewin.xz + 1 in
  let frame = Frame.{ (default_frame acs) with tl = acs.ttee; bl = acs.btee } in
  let win = Win.mk termwin 1 1 (yz - 2) (xz - 2) in
  let ui = Frame.mk ~frame acs termwin y x yz xz (Win.element win) in
  { ui }

let draw_static (view : t) : unit =
  Frame.draw view.ui;
  Frame.refresh view.ui
