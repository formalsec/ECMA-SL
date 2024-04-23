let run () (opt : Cmd_symbolic.options) =
  let n = Cmd_symbolic.run () opt in
  (* if n <> 0 then n *)
  if n <> () then ()
  else
    let filename = opt.filename in
    let testsuite = Fpath.(opt.workspace / "test-suite") in
    Cmd_replay.run () { filename; testsuite }
