let run (input_file : string) (execution_lang : Lang.t)
  (ecmaref_version : Ecmaref.version) (ecmaref_builder : Ecmaref.builder) : unit
    =
  let _ = input_file in
  let _ = execution_lang in
  let _ = ecmaref_version in
  let _ = ecmaref_builder in
  print_endline "NOT IMPLEMENTED!"

let main (debug : bool) (input_file : string) (execution_lang : Lang.t)
  (ecmaref_version : Ecmaref.version) (ecmaref_builder : Ecmaref.builder) : int
    =
  let _ = debug in
  let run' () = run input_file execution_lang ecmaref_version ecmaref_builder in
  Cmd.eval_cmd run'
