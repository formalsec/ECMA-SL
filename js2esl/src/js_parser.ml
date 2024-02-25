let from_file ~(filename : string) :
  ((Loc.t, Loc.t) Flow_ast.Program.t, Types.exit_value) Result.t =
  let (ast, errors) = Parser_flow.program filename in
  match errors with [] -> Ok ast | _ -> Error (`Parser_error errors)
