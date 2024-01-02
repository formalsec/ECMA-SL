let run (_ecmaref_version : Ecmaref.version) : unit =
  print_endline "NOT IMPLEMENTED!"

let main (_copts : Options.common_options) (ecmaref_version : Ecmaref.version) :
  int =
  Cmd.eval_cmd (fun () -> run ecmaref_version)
