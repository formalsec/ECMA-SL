let run (_ecmaref_version : Ecmaref.version) : unit =
  print_endline "NOT IMPLEMENTED!"

let main (_copts : Options.common_options) (ecmaref_version : Ecmaref.version) : int =
  let run' () = run ecmaref_version in
  Cmd.eval_cmd run'
