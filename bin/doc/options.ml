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

let untyped_flag =
  let doc = "Run ECMA-SL compiler without typechecking." in
  Arg.(value & flag & info [ "untyped" ] ~doc)

let target_esl_flag =
  let doc = "Interpret a program written in ECMA-SL (.esl)." in
  Arg.(value & flag & info [ "esl" ] ~doc)

let target_func =
  let doc = "The start function of the analysis." in
  Arg.(value & opt string "main" & info [ "target"; "t" ] ~doc)

let workspace_dir =
  let doc = "The workspace directory for the results of the analysis." in
  Arg.(value & opt string "ecma-out" & info [ "workspace"; "w" ] ~doc)

let testsuit_dir =
  let doc = "Search $(docv) for concrete testsuites to validate." in
  Arg.(required & pos 1 (some file) None & info [] ~docv:"DIR" ~doc)
