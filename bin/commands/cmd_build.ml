let run (ecmaref_version : Ecmaref.version) : unit =
  let _ = ecmaref_version in
  print_endline "NOT IMPLEMENTED!"

let main (debug : bool) (ecmaref_version : Ecmaref.version) : int =
  let _ = debug in
  let run' () = run ecmaref_version in
  Cmd.eval_cmd run'
