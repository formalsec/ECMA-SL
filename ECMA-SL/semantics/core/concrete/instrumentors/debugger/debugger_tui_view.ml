open Debugger_tui_helper

type t = { frame : Win.t Frame.t }

let create (acs : Acs.acs) (consolewin : Win.t) (codewin : Win.t) : t =
  let (y, x) = (0, codewin.xz - 1) in
  let yz = codewin.yz in
  let xz = consolewin.xz - codewin.xz + 1 in
  let border = Frame.{ (dflt_border acs) with tl = acs.ttee; bl = acs.btee } in
  let framewin = Win.mk consolewin y x yz xz in
  let win = Win.mk framewin 1 1 (yz - 2) (xz - 2) in
  let frame = Frame.mk ~border acs framewin (Win.element win) in
  { frame }

let resize (view : t) (consolewin : Win.t) (codewin : Win.t) : t =
  let (y, x) = (0, codewin.xz - 1) in
  let yz = codewin.yz in
  let xz = consolewin.xz - codewin.xz + 1 in
  let framewin = Win.mk consolewin y x yz xz in
  let win = Win.mk framewin 1 1 (yz - 2) (xz - 2) in
  let frame = Frame.resize view.frame framewin (Win.element win) in
  { frame }

let window (view : t) : window = Frame.window view.frame
let refresh (view : t) : unit = Frame.refresh view.frame
let rec element (view : t) : t element = { v = view; window; refresh; element }
let render_static (view : t) : unit = Frame.draw view.frame
