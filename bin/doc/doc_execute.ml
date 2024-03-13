open Cmdliner

let doc = "Executes a JavaScript program using the ECMARef interpreters"
let sdocs = Manpage.s_common_options

let description =
  [ "Given an encoded JavaScript program in Core ECMA-SL (.cesl), executes the \
     program using the JavaScript reference interpreter. When provided with an \
     unencoded JavaScript (.js) program, defaults to encoding the program into \
     Core ECMA-SL (.cesl) before execution."
  ; "The JavaScript reference interpreter (ECMARef interpreters) are written \
     in ECMA-SL, and adhere to the JavaScript standard line-by-line. The \
     option '--ecmaref' specifies the version of the standard in which the \
     program should be executed. Additionaly, the '--harness' flag can be used \
     to specify a JavaScript program that will run before the main program."
  ; "Some of the options of the 'interpret' command are also available. These \
     include the ability to run in verbose mode, enable the ECMA-SL debugger, \
     show the final result of the program, among others."
  ]

let man =
  [ `S Manpage.s_description
  ; `P (List.nth description 0)
  ; `P (List.nth description 1)
  ; `P (List.nth description 2)
  ]

let man_xrefs = [ `Page ("ecma-sl interpret", 1) ]

let exits =
  List.append Cmd.Exit.defaults
    [ Cmd.Exit.info ~doc:"on application failure" 1
    ; Cmd.Exit.info ~doc:"on generic execution error" 2
    ; Cmd.Exit.info ~doc:"on compilation error" 3
    ; Cmd.Exit.info ~doc:"on interpretation runtime error" 4
    ; Cmd.Exit.info ~doc:"on encoding error" 5
    ]

let options =
  Term.(
    const Cmd_execute.Options.set_options
    $ Options.File.input
    $ Options.File.harness
    $ Options.Execute.lang Cmd_execute.Options.langs
    $ Options.Execute.ecmaref
    $ Options.Interpret.trace
    $ Options.Interpret.trace_at
    $ Options.Interpret.debugger
    $ Options.Interpret.show_exitval )

let term = Term.(const Cmd_execute.main $ Options.Common.options $ options)
