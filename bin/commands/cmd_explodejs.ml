let run () (opts : Cmd_symbolic.Options.t) : unit Result.t =
  match Cmd_symbolic.run () opts with
  | Error _ as err -> err
  | Ok () ->
    let input = opts.input in
    let testsuite = Fpath.(opts.workspace / "test-suite") in
    Cmd_replay.run () { input; testsuite }
