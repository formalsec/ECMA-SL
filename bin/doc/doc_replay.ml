open Cmdliner

let doc = "Validates the symbolic testsuit"
let sdocs = Manpage.s_common_options
let description = "Replays concrete testsuites generated in symbolic execution."
let man = [ `S Manpage.s_description; `P description ]
let man_xrefs = []

let options =
  Term.(const Cmd_replay.options $ Options.File.input $ Options.testsuit_dir)

let term = Term.(const Cmd_replay.main $ options $ Options.Common.options)
