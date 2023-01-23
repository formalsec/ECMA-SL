let name = "ecma-se"
let version = "v0.1"
let banner () = print_endline (name ^ " " ^ version)
let usage = "Usage: " ^ name ^ " [option] [file ...]"

let argspec =
  Arg.align
    [
      ("-i", Arg.Set_string Flags.file, " read program from file");
      ("-o", Arg.Set_string Flags.workspace, " write result files to directory");
      ("-t", Arg.Set_string Flags.target, " target function to analyse");
      ("-v", Arg.Unit (fun () -> banner (); exit 0), " show version");
      ("--verbose", Arg.Set Flags.verbose, " verbose interpreter");
    ]

let plus_ext = ".esl"
let core_ext = ".cesl"

let dispatch_file_ext on_plus on_core file =
  if Filename.check_suffix file plus_ext then
    on_plus file
  else if Filename.check_suffix file core_ext then
    on_core file
  else
    raise (Sys_error (file ^ " :unreconized file type"))

let prog_of_plus file =
  let e_prog =
    Parsing_utils.(
      apply_prog_macros (resolve_prog_imports (
        parse_e_prog file (load_file file)))) in
  Compiler.compile_prog e_prog

let prog_of_core file =
  Parsing_utils.(parse_prog (load_file file))

let () =
  Printexc.record_backtrace true;
  try
    Arg.parse argspec (fun f -> Flags.file := f) usage;
    let testsuite_path = Filename.concat !Flags.workspace "test-suite" in
    Io.safe_mkdir testsuite_path;
    let prog = dispatch_file_ext prog_of_plus prog_of_core !Flags.file in
    let report = Eval.analyse prog !Flags.target
    and report_file = Filename.concat !Flags.workspace "report.json" in
    Io.write_file report_file (Report.report_to_json report);
    List.iter
      (fun (file, testcase) ->
        let file' = Filename.concat testsuite_path file in
        Io.write_file file' testcase)
      (Report.testsuite_to_json report)
  with exn ->
    flush_all ();
    prerr_endline
      (Sys.argv.(0) ^ ": uncaught exception " ^ Printexc.to_string exn);
    Printexc.print_backtrace stdout;
    exit 2
