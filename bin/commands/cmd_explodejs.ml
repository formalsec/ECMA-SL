let replay (opts : Cmd_symbolic.Options.t) : unit =
  let input = opts.input in
  let testsuite = Fpath.(opts.workspace / "test-suite") in
  Cmd_replay.run () { input; testsuite }

let run () (opts : Cmd_symbolic.Options.t) : unit =
  try
    Cmd_symbolic.run () opts;
    replay opts
  with _ -> ()
