open Cmdliner
open Options

let doc = "Interprets a Core ECMA-SL program"
let sdocs = Manpage.s_common_options

let description =
  [ "Given a Core ECMA-SL (.cesl) file, this command executes the program \
     using the concrete interpreter for Core ECMA-SL. If the program is \
     instead written in ECMA-SL (.esl), the '--esl' flag can be used with this \
     command to first compile the program to Core ECMA-SL before execution."
  ; "Some of the options from the 'compile' command are also available to be \
     used together with the '--esl' flag (.e.g, --untyped). When running the \
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
    const Cmd_interpret.options $ input_file $ interpret_esl_flag $ untyped_flag )

let term = Term.(const Cmd_interpret.main $ common_options $ options)
