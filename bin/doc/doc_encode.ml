open Cmdliner

let doc = "Encodes a JavaScript program in the ECMA-SL language"
let sdocs = Manpage.s_common_options

let description =
  [ "Given a JavaScript (.js) file, encodes the program in Core ECMA-SL \
     (.cesl). This process is done by generating the AST of the JavaScript \
     program, and then encode it in the Core ECMA-SL language."
  ]

let man = [ `S Manpage.s_description; `P (List.nth description 0) ]
let man_xrefs = []

let term =
  Term.(
    const Cmd_encode.main
    $ Options.common_options
    $ Options.input_file
    $ Options.output_file
    $ Options.encode_esl_flag
    $ Options.builder_func )
