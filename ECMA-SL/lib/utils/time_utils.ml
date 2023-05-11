open Core

let time_call (acc : float ref) (f : unit -> 'a) : 'a =
  let start = Caml.Sys.time () in
  let result = f () in
  acc := !acc +. Caml.Sys.time () -. start;
  result
