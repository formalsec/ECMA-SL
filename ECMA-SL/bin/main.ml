open Cmdliner
open Ecma_sl

let help_sec =
  [ `S Manpage.s_common_options
  ; `P "These options are common to all commands."
  ; `S "MORE HELP"
  ; `P "Use $(mname) $(i,COMMAND) --help for help on a single command."
  ; `Noblank
  ; `S Manpage.s_bugs
  ; `P "Check bug reports at https://github.com/formalsec/ECMA-SL/issues."
  ]

let copts debug = debug

let copts_t =
  let docs = Manpage.s_common_options in
  let debug =
    let doc = "Give debug output." in
    Arg.(value & flag & info [ "debug" ] ~docs ~doc)
  in
  Term.(const copts $ debug)

let sdocs = Manpage.s_common_options

let file =
  let doc = "file to analyse" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let target =
  let doc = "target function to analyse" in
  Arg.(value & opt string "main" & info [ "target"; "d" ] ~doc)

let workspace =
  let doc = "write result file to directory" in
  Arg.(value & opt string "ecma-out" & info [ "workspace"; "o" ] ~doc)

let compile_cmd =
  let output =
    let doc = "write the program to this file" in
    Arg.(value & opt (some string) None & info [ "output"; "o" ] ~doc)
  in
  let unsafe =
    let doc = "skip typechecker" in
    Arg.(value & flag & info [ "untyped" ] ~doc)
  in
  let doc = "Compiler from plus to core" in
  let man =
    [ `S Manpage.s_description
    ; `P "ECMA-SL compiler command."
    ; `P "Currently only does .esl->.cesl but we should also do .js->.cesl"
    ; `Blocks help_sec
    ]
  in
  let info = Cmd.info "compile" ~doc ~sdocs ~man in
  Cmd.v info Term.(const Cmd_run.compile $ copts_t $ unsafe $ file $ output)

let run_cmd =
  let open Cmdliner in
  let doc = "ECMA-SL Interpreter" in
  let man =
    [ `S Manpage.s_description
    ; `P "ECMA-SL Concrete Interpreter"
    ; `Blocks help_sec
    ]
  in
  let info = Cmd.info "run" ~doc ~sdocs ~man in
  Cmd.v info Term.(const Cmd_run.main $ copts_t $ file)

let sym_cmd =
  let open Cmdliner in
  let doc = "Symbolic execution analysis" in
  let man =
    [ `S Manpage.s_description
    ; `P "Symbolic analysis of ECMA-SL."
    ; `P "Parses files with respect to their file extension .js/.esl/.cesl."
    ; `Blocks help_sec
    ]
  in
  let info = Cmd.info "sym" ~doc ~sdocs ~man in
  Cmd.v info Term.(const Cmd_sym.main $ copts_t $ target $ workspace $ file)

let val_cmd =
  let open Cmdliner in
  let file =
    let doc = "symbolic test to validate" in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)
  in
  let dir =
    let doc = "uses concrete testsuite in directory $(docv)" in
    Arg.(required & pos 1 (some file) None & info [] ~docv:"DIR" ~doc)
  in
  let doc = "Testsuite validation" in
  let man =
    [ `S Manpage.s_description
    ; `P "Validates symbolic tests produced by explode.js"
    ; `Blocks help_sec
    ]
  in
  let info = Cmd.info "validate" ~doc ~sdocs ~man in
  Cmd.v info Term.(const Cmd_sym.validate $ copts_t $ file $ dir)

let cli =
  let doc = "ECMA-SL" in
  let man = help_sec in
  let info = Cmd.info "ecma-sl" ~version:"%%VERSION%%" ~doc ~sdocs ~man in
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ copts_t)) in
  Cmd.group info ~default [ compile_cmd; run_cmd; sym_cmd; val_cmd ]

let () =
  Printexc.record_backtrace true;
  try exit (Cmdliner.Cmd.eval' cli)
  with exn ->
    flush_all ();
    Printexc.print_backtrace stdout;
    Format.eprintf "%s: uncaught exception %s@." Sys.argv.(0)
      (Printexc.to_string exn);
    exit 1
