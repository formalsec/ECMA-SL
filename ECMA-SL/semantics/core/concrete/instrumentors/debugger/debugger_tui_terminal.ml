open Debugger_tui_helper

type t = { ui : Win.t }

let make (termwin : Win.t) (execwin : Win.t) : t =
  let (y, x) = (execwin.yz - 1, execwin.xz) in
  let yz = termwin.yz - y in
  let xz = termwin.xz - x in
  let ui = Win.mk termwin y x yz xz in
  { ui }

let draw (_term : t) : unit = ()
