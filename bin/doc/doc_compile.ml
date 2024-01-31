open Cmdliner

let doc = "Compiles an ECMA-SL program to Core ECMA-SL"
let sdocs = Manpage.s_common_options

let description =
  [ "Given an ECMA-SL (.esl) file, compiles the program to Core ECMA-SL \
     (.cesl) language."
  ]

let man = [ `S Manpage.s_description; `P (List.nth description 0) ]
let man_xrefs = []

let exits =
  List.append Cmd.Exit.defaults
    [ Cmd.Exit.info ~doc:"on application failure" 1
    ; Cmd.Exit.info ~doc:"on generic execution error" 2
    ; Cmd.Exit.info ~doc:"on compilation error" 3
    ]

let cmd_options input_file output_file untyped : Cmd_compile.options =
  { input_file; output_file; untyped }

let options =
  Term.(
    const cmd_options
    $ Options.File.input
    $ Options.File.output
    $ Options.Compile.untyped )

let term = Term.(const Cmd_compile.main $ Options.Common.options $ options)
