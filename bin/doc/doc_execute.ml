open Cmdliner

let doc = "Executes a JavaScript program using the ECMARef interpreters"
let sdocs = Manpage.s_common_options

let description =
  [ "Given a JavaScript program encoded in Core ECMA-SL (.cesl), executes the \
     program using the JavaScript reference interpreters. When provided with \
     an unencoded JavaScript (.js) program, the default behavior is to encode \
     the program into Core ECMA-SL (.cesl) before execution."
  ; "The JavaScript reference interpreter (ECMARef interpreters) are written \
     in ECMA-SL, and adhere to the JavaScript standard line-by-line. Use the \
     '--ecmaref' option to specify the version of the standard in which the \
     program should be executed."
  ; "Some of the options from the 'interpreter' command are also available on \
     this command. These include the ability to run in verbose mode, enable \
     the ECMA-SL debugger, show the final result of the program, among others."
  ]

let man =
  [ `S Manpage.s_description
  ; `P (List.nth description 0)
  ; `P (List.nth description 1)
  ; `P (List.nth description 2)
  ]

let man_xrefs = [ `Page ("ecma-sl interpret", 2) ]

let exits =
  List.append Cmd.Exit.defaults
    [ Cmd.Exit.info ~doc:"on application failure" 1
    ; Cmd.Exit.info ~doc:"on generic execution error" 2
    ; Cmd.Exit.info ~doc:"on encoding error" 3
    ; Cmd.Exit.info ~doc:"on interpretation runtime error" 4
    ]

let cmd_options input harness execute_lang execute_version interpret_verbose
  interpret_verbose_at interpret_debugger interpret_show_result
  interpret_hide_prints : Cmd_execute.options =
  { input
  ; harness
  ; execute_lang
  ; execute_version
  ; interpret_verbose
  ; interpret_verbose_at
  ; interpret_debugger
  ; interpret_show_result
  ; interpret_hide_prints
  }

let options =
  Term.(
    const cmd_options
    $ Options.File.input
    $ Options.File.harness
    $ Options.Execute.lang Cmd_execute.langs
    $ Options.Execute.version
    $ Options.Interpret.verbose
    $ Options.Interpret.verbose_at
    $ Options.Interpret.debugger
    $ Options.Interpret.show_result
    $ Options.Interpret.hide_prints )

let term = Term.(const Cmd_execute.main $ Options.Common.options $ options)
