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
       from ECMA-SL to Core ECMA-SL, a simplified version of the platform's \
       language, as well as an interpreter for Core ECMA-SL. By combining \
       these tools, one can execute a JavaScript program using the reference \
       interpreters for JavaScript."
    ; "Use ecma-sl <command> --help for more information on a specific command."
    ]

  let man =
    [ `S Cmdliner.Manpage.s_description
    ; `P (List.nth description 0)
    ; `P (List.nth description 1)
    ; `P (List.nth description 2)
    ; `S Manpage.s_common_options
    ; `P "These options are common to all commands."
    ; `S Manpage.s_bugs
    ; `P "Check bug reports at https://github.com/formalsec/ECMA-SL/issues."
    ]

  let man_xrefs = []

  let exits =
    List.append Cmd.Exit.defaults
      [ Cmd.Exit.info ~doc:"on application failure" 1
      ; Cmd.Exit.info ~doc:"on generic execution error" 2
      ]
end

let compile_cmd =
  let open Doc_compile in
  let info = Cmd.info "compile" ~doc ~sdocs ~man ~man_xrefs ~exits in
  Cmd.v info term

let interpret_cmd =
  let open Doc_interpret in
  let info = Cmd.info "interpret" ~doc ~sdocs ~man ~man_xrefs ~exits in
  Cmd.v info term

let encode_cmd =
  let open Doc_encode in
  let info = Cmd.info "encode" ~doc ~sdocs ~man ~man_xrefs in
  Cmd.v info term

let execute_cmd =
  let open Doc_execute in
  let info = Cmd.info "execute" ~doc ~sdocs ~man ~man_xrefs in
  Cmd.v info term

let build_cmd =
  let open Doc_build in
  let info = Cmd.info "build" ~doc ~sdocs ~man ~man_xrefs in
  Cmd.v info term

let symbolic_cmd =
  let open Doc_symbolic in
  let info = Cmd.info "symbolic" ~doc ~sdocs ~man ~man_xrefs in
  Cmd.v info term

let replay_cmd =
  let open Doc_replay in
  let info = Cmd.info "replay" ~doc ~sdocs ~man ~man_xrefs in
  Cmd.v info term

let cmd_list =
  [ compile_cmd
  ; interpret_cmd
  ; encode_cmd
  ; execute_cmd
  ; build_cmd
  ; symbolic_cmd
  ; replay_cmd
  ]

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
    Format.eprintf "%s: uncaught exception %s@." Sys.argv.(0)
      (Printexc.to_string exn);
    Printexc.print_backtrace stderr;
    exit 1
