open Cmdliner

let doc = "Compiles an ECMA-SL program to Core ECMA-SL"
let sdocs = Manpage.s_common_options

let description =
  [ "Given an ECMA-SL (.esl) file, compiles the program to Core ECMA-SL \
     (.cesl)."
  ]

let man = [ `S Manpage.s_description; `P (List.nth description 0) ]
let man_xrefs = []

let term =
  Term.(
    const Cmd_compile.main
    $ Options.common_options
    $ Options.input_file
    $ Options.output_file
    $ Options.untyped_flag )
