let run () (opt : Cmd_symbolic.Options.t) =
  let n = Cmd_symbolic.run () opt in
  (* if n <> 0 then n *)
  if n <> () then ()
  else
    let filename = opt.input in
    let testsuite = Fpath.(opt.workspace / "test-suite") in
    Cmd_replay.run () { filename; testsuite }
