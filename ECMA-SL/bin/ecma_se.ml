open Core

let plus_ext = ".esl"
let core_ext = ".cesl"
let js_ext = ".js"

let dispatch_file_ext on_plus on_core on_js file =
  if Filename.check_suffix file plus_ext then on_plus file
  else if Filename.check_suffix file core_ext then on_core file
  else if Filename.check_suffix file js_ext then on_js file
  else raise (Sys_error (file ^ " :unreconized file type"))

let prog_of_plus file =
  let e_prog =
    Parsing_utils.(
      apply_prog_macros
        (resolve_prog_imports (parse_e_prog file (load_file file))))
  in
  Compiler.compile_prog e_prog

let prog_of_core file = Parsing_utils.(parse_prog (load_file file))

let prog_of_js interp file =
  assert (Sys_unix.file_exists_exn interp);
  let ast_file = Filename.chop_extension file in
  let ret =
    Sys_unix.command
      (String.concat ~sep:" "
         [ "js2ecma-sl"; "-c"; "-i"; file; "-o"; ast_file ])
  in
  if ret <> 0 then raise (Sys_error ("unable to compile: " ^ file))
  else
    let ast_str = In_channel.read_all ast_file in
    let interp = In_channel.read_all interp in
    let program = String.concat ~sep:";\n" [ ast_str; interp ] in
    Sys_unix.remove ast_file;
    Parsing_utils.parse_prog program

let error at category msg =
  prerr_endline (Source.string_of_region at ^ ":" ^ category ^ ":" ^ msg)

let run_prog prog target =
  try Eval.main prog target with
  | Eval.Crash (at, msg) -> error at "runtime crash" msg
  | Eval.Invalid_arg (at, msg) -> error at "invalid arg" msg
  | exn -> raise exn

let command_parameters : (unit -> unit) Command.Param.t =
  let%map_open.Command files =
    anon (sequence ("filename" %: Filename_unix.arg_type))
  and target =
    flag "target" ~aliases:[ "d" ]
      (optional_with_default "main" string)
      ~doc:"string target function to analyse"
  and workspace =
    flag "workspace" ~aliases:[ "o" ]
      (optional_with_default "output" string)
      ~doc:"string write result files to directory"
  and policy =
    flag "policy"
      (optional_with_default "breadth" string)
      ~doc:"string search policy (depth|breadth|random)"
  and interp =
    flag "interp"
      (optional_with_default "es6.cesl" string)
      ~doc:"path to ECMAScript interpreter"
  and verbose = flag "verbose" no_arg ~doc:" verbose interpreter" in
  fun () ->
    Config.target := target;
    Config.workspace := workspace;
    Config.policy := policy;
    Log.on_debug := verbose;
    List.iter files ~f:(fun f ->
        Config.file := f;
        let prog =
          dispatch_file_ext prog_of_plus prog_of_core (prog_of_js interp) f
        in
        run_prog prog target)

let command =
  Command.basic ~summary:"ECMA-SL symbolic analysis" command_parameters

let () =
  Backtrace.Exn.set_recording true;
  try Command_unix.run ~version:"0.1.0" command
  with exn ->
    Caml.flush_all ();
    Printexc.print_backtrace stdout;
    fprintf stderr "%s: uncaught exception %s"
      (Sys.get_argv ()).(0)
      (Exn.to_string exn);
    exit 2
