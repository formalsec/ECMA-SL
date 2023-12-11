open Cmdliner

module AppInfo = struct
  let version = "%%VERSION%%"
  let doc = "Executable specification of the ECMAScript standard"
  let sdocs = Manpage.s_common_options

  let description =
    [ "ECMA-SL is a comprehensive platform designed for the specification and \
       execution of the ECMAScript standard, commonly known as JavaScript. The \
       platform introduces an intermediate language, ECMA-SL, which serves as \
       a bridge between JavaScript and its execution environment. This \
       intermediate language is used to provide a reference implementation of \
       the ECMAScript standard that adheres to JavaScript's specification."
    ; "Key features of the platform include a JavaScript-to-ECMA-SL \
       (JS2ECMA-SL) parser, allowing the conversion of JavaScript code into \
       the ECMA-SL language. Additionally, ECMA-SL incorporates a compiler \
       from ECMA-SL to Core ECMA-SL, a simplified version of the intermediate \
       language, as well as an interpreter for Core ECMA-SL. The combination \
       of these tools results in a mechanism to execute JavaScript programs \
       using the reference interpreters for the language."
    ; "The ECMA-SL platform also includes a symbolic analysis mechanism for \
       performing symbolic analyses on ECMA-SL programs. [FIXME?]"
    ; "Use ecma-sl <command> --help for more information on a specific command."
    ]

  let man =
    [ `S Cmdliner.Manpage.s_description
    ; `P (List.nth description 0)
    ; `P (List.nth description 1)
    ; `P (List.nth description 2)
    ; `P (List.nth description 3)
    ; `S Manpage.s_common_options
    ; `P "These options are common to all commands."
    ; `S Manpage.s_bugs
    ; `P "Check bug reports at https://github.com/formalsec/ECMA-SL/issues."
    ]

  let man_xrefs = []

  let exits =
    List.append Cmd.Exit.defaults
      [ Cmd.Exit.info ~doc:"on failure" 1
      ; Cmd.Exit.info ~doc:"on error" 2
      ; Cmd.Exit.info ~doc:"on unsupported" 3
      ]
end

let compile_cmd =
  let open Doc_compile in
  let info = Cmd.info "compile" ~doc ~sdocs ~man ~man_xrefs in
  Cmd.v info term

let execute_cmd =
  let open Doc_interpret in
  let info = Cmd.info "interpret" ~doc ~sdocs ~man ~man_xrefs in
  Cmd.v info term

let symbolic_cmd =
  let open Doc_symbolic in
  let info = Cmd.info "symbolic" ~doc ~sdocs ~man ~man_xrefs in
  Cmd.v info term

let replay_cmd =
  let open Doc_replay in
  let info = Cmd.info "replay" ~doc ~sdocs ~man ~man_xrefs in
  Cmd.v info term

let cmd_list = [ compile_cmd; execute_cmd; symbolic_cmd; replay_cmd ]

let main_cmd =
  let open AppInfo in
  let info = Cmd.info "ecma-sl" ~version ~doc ~sdocs ~man ~man_xrefs ~exits in
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  Cmd.group info ~default cmd_list

let () =
  Printexc.record_backtrace true;
  try exit (Cmdliner.Cmd.eval' main_cmd)
  with exn ->
    flush_all ();
    Printexc.print_backtrace stdout;
    Format.eprintf "%s: uncaught exception %s@." Sys.argv.(0)
      (Printexc.to_string exn);
    exit 1
