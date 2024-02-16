open Cmdliner

let doc = "TODO"
let sdocs = Manpage.s_common_options
let description = [ "TODO" ]
let man = [ `S Manpage.s_description; `P (List.nth description 0) ]
let man_xrefs = []
let options = Term.(const Cmd_sink.options $ Options.Fpath_.input2)
let term = Term.(const Cmd_sink.main $ Options.Common.options $ options)
