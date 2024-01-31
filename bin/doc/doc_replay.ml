open Cmdliner

let doc = "Validates the symbolic testsuit"
let sdocs = Manpage.s_common_options

let description =
  [ "Replays concrete testsuites generated in symbolic execution." ]

let man = [ `S Manpage.s_description; `P (List.nth description 0) ]
let man_xrefs = []

let term =
  Term.(
    const Cmd_symbolic.validate
    $ Options.Common.options
    $ Options.File.input
    $ Options.testsuit_dir )
