open Cmdliner

let doc = "Explode.js symbolic vulnerability confirmation engine"
let sdocs = Manpage.s_common_options
let description = [ (`P0, "Tries to blow stuff up") ]
let man = [ `S Manpage.s_description; `P (List.assoc `P0 description) ]
let man_xrefs = []

let options =
  Term.(
    const Cmd_symbolic.options
    $ Options.Fpath_.input
    $ Options.target_func
    $ Options.workspace_dir )

let term = Term.(const Cmd_explodejs.main $ Options.Common.options $ options)
