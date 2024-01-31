open Cmdliner

let doc = "Encodes a JavaScript program in the Core ECMA-SL language"
let sdocs = Manpage.s_common_options

let description =
  [ "Given a JavaScript (.js) file, encodes the program in Core ECMA-SL \
     (.cesl). This is done through a two-stage process. First, generate the \
     Abstract Syntax Tree (AST) of the JavaScript program using Esprima, an \
     official JavaScript parser. Subsequently, translate the resulting AST \
     into the Core ECMA-SL language, introducing minor adjustments to meet the \
     requirements of ECMARef interpreters."
  ]

let man = [ `S Manpage.s_description; `P (List.nth description 0) ]
let man_xrefs = []

let term =
  Term.(
    const Cmd_encode.main
    $ Options.Common.options
    $ Options.File.input
    $ Options.File.output
    $ Options.encode_esl_flag
    $ Options.builder_func )
