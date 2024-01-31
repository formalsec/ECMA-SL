open Cmdliner
open Options

let doc = "Interprets a Core ECMA-SL program"
let sdocs = Manpage.s_common_options

let description =
  [ "Given an ECMA-SL (.cesl) or Core ECMA-SL (.cesl) file, this command \
     executes the program using the concrete interpreter for Core ECMA-SL. If \
     the program is written in ECMA-SL (.esl), the command defaults to \
     compiling program into Core ECMA-SL (.cesl)."
  ; "Some of the options from the 'compile' command are also available when \
     running the command on an ECMA-SL (.esl) file. When running the \
     interpreter directly on a Core ECMA-SL file, these options are ignored."
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

let options =
  Term.(
    const Cmd_interpret.options
    $ input_file
    $ interpret_lang Cmd_interpret.langs
    $ interpret_verbose_flag
    $ interpret_verbose_at_flag
    $ interpret_debugger_flag
    $ compile_untyped_flag )

let term = Term.(const Cmd_interpret.main $ common_options $ options)
