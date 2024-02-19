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

let exits =
  List.append Cmd.Exit.defaults
    [ Cmd.Exit.info ~doc:"on application failure" 1
    ; Cmd.Exit.info ~doc:"on generic execution error" 2
    ; Cmd.Exit.info ~doc:"on encoding error" 3
    ]

let cmd_options inputs output builder : Cmd_encode.options =
  { inputs; output; builder }

let options =
  Term.(
    const cmd_options
    $ Options.File.inputs
    $ Options.File.output
    $ Options.Encode.builder )

let term = Term.(const Cmd_encode.main $ Options.Common.options $ options)
