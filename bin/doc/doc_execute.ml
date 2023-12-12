open Cmdliner

let doc = "Executes a JavaScript program in the ECMA-SL pipeline"
let sdocs = Manpage.s_common_options

let description =
  [ "Given a JavaScript (.js) file, executes the program using the ECMA-SL \
     pipeline. This includes encoding the program in Core ECMA-SL (.cesl), and \
     then evaluating it using a reference interpreter."
  ; "Besides executing JavaScript programs, the execution engine can execute \
     programs already encoded in ECMA-SL or Core ECMA-SL. By default, the \
     execution pipeline depends on the extension of the input file. This can \
     be changed using the '--lang' option."
  ; "Additionaly, one can specify the version of the reference interpreter to \
     use (--ecmaref), as well as options to automatically build the \
     interpreter (--ecmaref-build)."
  ]

let man =
  [ `S Manpage.s_description
  ; `P (List.nth description 0)
  ; `P (List.nth description 1)
  ]

let man_xrefs = []

let term =
  Term.(
    const Cmd_execute.main
    $ Options.common_options
    $ Options.input_file
    $ Options.execution_lang
    $ Options.ecmaref_version
    $ Options.ecmaref_builder )
