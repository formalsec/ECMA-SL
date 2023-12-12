open Cmdliner

let debug_flag =
  let doc = "Run and generate debug output." in
  Arg.(value & flag & info [ "debug" ] ~doc)

let common_options =
  let common_options' opt = opt in
  Term.(const common_options' $ debug_flag)

let input_file =
  let doc = "Name of the input file." in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"FILE")

let output_file =
  let doc = "Name of the output file." in
  Arg.(value & opt (some string) None & info [ "o"; "output" ] ~doc)

let heap_file =
  let doc = "Name of the heap file (to write the final heap of the program)." in
  Arg.(value & opt (some string) None & info [ "h"; "heap" ] ~doc)

let execution_lang =
  let open Lang in
  let doc = "Language of the program to be executed." in
  let langs_enum = Arg.enum [ auto; js; esl; cesl ] in
  Arg.(value & opt langs_enum Auto & info [ "lang" ] ~doc)

let ecmaref_version =
  let open Ecmaref in
  let doc = "Version of the reference interpreter." in
  let ecmaref_version_enum = Arg.enum [ latest; toyecma; ecmaref5; ecmaref6 ] in
  Arg.(value & opt ecmaref_version_enum Latest & info [ "ecmaref" ] ~doc)

let ecmaref_builder =
  let open Ecmaref in
  let doc = "Building options for the reference interpreter." in
  let ecmaref_builder_enum = Arg.enum [ never; if_missing; always ] in
  Arg.(value & opt ecmaref_builder_enum Never & info [ "ecmaref-builder" ] ~doc)

let interpret_esl_flag =
  let doc = "Interpret a program written in ECMA-SL (.esl)." in
  Arg.(value & flag & info [ "esl" ] ~doc)

let encode_esl_flag =
  let doc = "Encode the program in ECMA-SL (.esl)." in
  Arg.(value & flag & info [ "esl" ] ~doc)

let untyped_flag =
  let doc = "Run ECMA-SL compiler without typechecking." in
  Arg.(value & flag & info [ "untyped" ] ~doc)

let target_func =
  let doc = "The start function of the analysis." in
  Arg.(value & opt string "main" & info [ "target"; "t" ] ~doc)

let builder_func =
  let doc = "Name of the function that builds the AST." in
  Arg.(value & opt string "Build_AST" & info [ "builder" ] ~doc)

let workspace_dir =
  let doc = "The workspace directory for the results of the analysis." in
  Arg.(value & opt string "ecma-out" & info [ "workspace"; "w" ] ~doc)

let testsuit_dir =
  let doc = "Search $(docv) for concrete testsuites to validate." in
  Arg.(required & pos 1 (some file) None & info [] ~docv:"DIR" ~doc)
