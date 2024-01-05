let run (_input_file : string) (_execution_lang : Lang.t)
  (_ecmaref_version : Ecmaref.version) (_ecmaref_builder : Ecmaref.builder) :
  unit =
  print_endline "NOT IMPLEMENTED!"

let main (_copts : Options.common_options) (input_file : string)
  (execution_lang : Lang.t) (ecmaref_version : Ecmaref.version)
  (ecmaref_builder : Ecmaref.builder) : int =
  let run' () = run input_file execution_lang ecmaref_version ecmaref_builder in
  Cmd.eval_cmd run'
