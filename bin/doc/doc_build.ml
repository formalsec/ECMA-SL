open Cmdliner

let doc = "Builds the reference interpreter."
let sdocs = Manpage.s_common_options

let description =
  [ "Given a version of the ECMAScript standard, builds the reference \
     interpreter for that specific version. The resulting interpreter is \
     stored internally within the ECMA-SL pipeline."
  ]

let man = [ `S Manpage.s_description; `P (List.nth description 0) ]
let man_xrefs = []

let term =
  Term.(const Cmd_build.main $ Options.common_options $ Options.ecmaref_version)
