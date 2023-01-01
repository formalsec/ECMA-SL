let time_call acc f =
  let start = Sys.time () in
  let result = f () in
  acc := !acc +. (Sys.time ()) -. start;
  result
