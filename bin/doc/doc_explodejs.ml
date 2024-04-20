open Cmdliner

let doc = "Explode.js symbolic vulnerability confirmation engine"
let sdocs = Manpage.s_common_options
let description = "Tries to blow stuff up"
let man = [ `S Manpage.s_description; `P description ]
let man_xrefs = []

let exits =
  [ Cmd.Exit.info ~doc:"on application failure" 1
  ; Cmd.Exit.info ~doc:"on unsupported/unknown vulnerability type" 2
  ; Cmd.Exit.info ~doc:"on unsupported/malformed taint summary" 3
  ; Cmd.Exit.info ~doc:"on internal timeout (when set)" 4
  ]

let filename =
  let doc = "Overwrite input file in taint_summary" in
  Arg.(value & opt (some Options.File.fpath) None & info [ "filename" ] ~doc)

let time_limit =
  let doc = "Maximum time limit for analysis" in
  Arg.(value & opt float 0.0 & info [ "timeout" ] ~doc)

let options =
  Term.(
    const Cmd_explodejs.options
    $ Options.File.input
    $ filename
    $ Options.workspace_dir
    $ time_limit)

let term = Term.(const Cmd_explodejs.main $ options $ Options.Common.options)
