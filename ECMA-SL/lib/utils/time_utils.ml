let time_call (acc : float ref) (f : unit -> 'a) : 'a =
  let start = Sys.time () in
  let result = f () in
  acc := !acc +. Sys.time () -. start;
  result
