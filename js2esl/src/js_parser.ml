let from_file ~(filename : string) :
  ((Loc.t, Loc.t) Flow_ast.Program.t, Types.exit_value) Result.t =
  In_channel.with_open_text filename (fun chan ->
      let program_text = In_channel.input_all chan in
      let (ast, errors) = Parser_flow.program program_text in
      match errors with [] -> Ok ast | _ -> Error (`Parser_error errors) )
