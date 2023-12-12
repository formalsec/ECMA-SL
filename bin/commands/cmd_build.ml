let run (_ecmaref_version : Ecmaref.version) : unit =
  print_endline "NOT IMPLEMENTED!"

let main (_debug : bool) (ecmaref_version : Ecmaref.version) : int =
  let run' () = run ecmaref_version in
  Cmd.eval_cmd run'
