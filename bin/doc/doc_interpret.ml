open Cmdliner

let doc = "Interprets a Core ECMA-SL program"
let sdocs = Manpage.s_common_options

let description =
  [ "Given a Core ECMA-SL (.cesl) file, this command executes the program \
     using the Core ECMA-SL concrete interpreter. If the program is instead \
     written in ECMA-SL (.esl), the '--esl' flag can be used with this command \
     to first compile the program to Core ECMA-SL before execution."
  ; "Some of the options from the 'compile' command are also available to be \
     used together with the '--esl' flag (.e.g, --untyped). When running the \
     interpreter directly on a Core ECMA-SL file, these options are ignored."
  ]

let man =
  [ `S Manpage.s_description
  ; `P (List.nth description 0)
  ; `P (List.nth description 1)
  ]

let man_xrefs = []

let term =
  Term.(
    const Cmd_interpret.main
    $ Options.common_options
    $ Options.input_file
    $ Options.heap_file
    $ Options.target_esl_flag
    $ Options.untyped_flag )
