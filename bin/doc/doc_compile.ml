open Cmdliner
open Options

let doc = "Compiles an ECMA-SL program to Core ECMA-SL"
let sdocs = Manpage.s_common_options

let description =
  [ "Given an ECMA-SL (.esl) file, compiles the program to Core ECMA-SL \
     (.cesl)."
  ]

let man = [ `S Manpage.s_description; `P (List.nth description 0) ]
let man_xrefs = []

let options =
  Term.(const Cmd_compile.options $ input_file $ output_file $ untyped_flag)

let term = Term.(const Cmd_compile.main $ common_options $ options)
