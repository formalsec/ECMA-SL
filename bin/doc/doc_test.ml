open Cmdliner

let doc = "Runs a JavaScript test on the ECMARef interpreters"
let sdocs = Manpage.s_common_options

let description =
  [ "Given a JavaScript test encoded in Core ECMA-SL (.cesl), executes the \
     test using the JavaScript reference interpreters. When provided with an \
     unencoded JavaScript (.js) test, the default behavior is to encode the \
     test into Core ECMA-SL (.cesl) before execution."
  ; "Running a JavaScript test is similar to executing a regular JavaScript \
     program, with some distinctions. All program prints are discarded, and \
     the test's return value is analyzed to determine success or failure. As a \
     result, most of the options from the 'execute' command are also available \
     on this command."
  ]

let man =
  [ `S Manpage.s_description
  ; `P (List.nth description 0)
  ; `P (List.nth description 1)
  ]

let man_xrefs = [ `Page ("ecma-sl execute", 2) ]

let exits =
  List.append Cmd.Exit.defaults
    [ Cmd.Exit.info ~doc:"on application failure" 1
    ; Cmd.Exit.info ~doc:"on generic execution error" 2
    ; Cmd.Exit.info ~doc:"on encoding error" 3
    ; Cmd.Exit.info ~doc:"on interpretation runtime error" 4
    ; Cmd.Exit.info ~doc:"on test error" 5
    ]

let cmd_options inputs harness execute_lang execute_version : Cmd_test.options =
  { inputs; harness; execute_lang; execute_version }

let options =
  Term.(
    const cmd_options
    $ Options.File.inputs
    $ Options.File.harness
    $ Options.Execute.lang Cmd_execute.langs
    $ Options.Execute.version )

let term = Term.(const Cmd_test.main $ Options.Common.options $ options)
