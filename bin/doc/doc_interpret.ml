open Cmdliner

let doc = "Interprets a Core ECMA-SL program"
let sdocs = Manpage.s_common_options

let description =
  [ "Given an ECMA-SL (.cesl) or Core ECMA-SL (.cesl) file, executes the \
     program using the concrete interpreter for Core ECMA-SL. When provided \
     with an ECMA-SL (.esl) file, defaults to compiling the program into Core \
     ECMA-SL (.cesl) before execution."
  ; "Some of the options from the 'compile' command are also available when \
     interpreting an ECMA-SL (.esl) file. When running the interpreter \
     directly on a Core ECMA-SL file, these options are ignored."
  ]

let man =
  [ `S Manpage.s_description
  ; `P (List.nth description 0)
  ; `P (List.nth description 1)
  ]

let man_xrefs = [ `Page ("ecma-sl compile", 2) ]

let exits =
  List.append Cmd.Exit.defaults
    [ Cmd.Exit.info ~doc:"on application failure" 1
    ; Cmd.Exit.info ~doc:"on generic execution error" 2
    ; Cmd.Exit.info ~doc:"on compilation error" 3
    ; Cmd.Exit.info ~doc:"on interpretation runtime error" 4
    ]

let cmd_options input_file interpret_lang interpret_verbose interpret_verbose_at
  interpret_debugger untyped : Cmd_interpret.options =
  { input_file
  ; interpret_lang
  ; interpret_verbose
  ; interpret_verbose_at
  ; interpret_debugger
  ; untyped
  }

let options =
  Term.(
    const cmd_options
    $ Options.File.input
    $ Options.Interpret.lang Cmd_interpret.langs
    $ Options.Interpret.verbose
    $ Options.Interpret.verbose_at
    $ Options.Interpret.debugger
    $ Options.Compile.untyped )

let term = Term.(const Cmd_interpret.main $ Options.Common.options $ options)
