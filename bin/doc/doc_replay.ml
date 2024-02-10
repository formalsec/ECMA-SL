open Cmdliner

let doc = "Validates the symbolic testsuit"
let sdocs = Manpage.s_common_options

let description =
  [ (`P0, "Replays concrete testsuites generated in symbolic execution.") ]

let man = [ `S Manpage.s_description; `P (List.assoc `P0 description) ]
let man_xrefs = []

let term =
  Term.(
    const Cmd_symbolic.validate
    $ Options.Common.options
    $ Options.Fpath_.input
    $ Options.testsuit_dir )
