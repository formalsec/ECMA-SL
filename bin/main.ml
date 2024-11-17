open Cmdliner

let set_copts (debug : Enums.DebugLvl.t) (colorless : bool) : unit =
  Ecma_sl.Font.Config.colored := not colorless;
  Ecma_sl.Log.Config.log_warns := debug >= Warn;
  Ecma_sl.Log.Config.log_debugs := debug >= Full

let copts =
  let open Term in
  const set_copts $ Docs.CommonOpts.debug $ Docs.CommonOpts.colorless

let compile_opts =
  let open Term in
  const Cmd_compile.Options.set
  $ Docs.FileOpts.input
  $ Docs.FileOpts.output
  $ Docs.CompileOpts.untyped

let compile_cmd =
  let open Docs.CompileCmd in
  let info = Cmd.(info "compile" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(const Cmd_compile.run $ copts $ compile_opts) in
  Cmd.v info term

let interpreter_instrument =
  let open Term in
  const Cmd_interpret.Options.set_instrument
  $ Docs.InterpretOpts.tracer
  $ Docs.InterpretOpts.tracer_loc
  $ Docs.InterpretOpts.tracer_depth
  $ Docs.InterpretOpts.debugger
  $ Docs.InterpretOpts.profiler

let interpreter_config =
  let open Term in
  const Cmd_interpret.Options.set_config
  $ Docs.InterpretOpts.print_depth
  $ Docs.InterpretOpts.show_exitval
  $ interpreter_instrument

let interpret_opts =
  let open Term in
  const Cmd_interpret.Options.set
  $ Docs.FileOpts.input
  $ Docs.InterpretOpts.lang
  $ Docs.InterpretOpts.main
  $ Docs.CompileOpts.untyped
  $ interpreter_config

let interpret_cmd =
  let open Docs.InterpretCmd in
  let info = Cmd.(info "interpret" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(const Cmd_interpret.run $ copts $ interpret_opts) in
  Cmd.v info term

let encode_opts =
  let open Term in
  const Cmd_encode.Options.set
  $ Docs.FileOpts.inputs
  $ Docs.FileOpts.output
  $ Docs.EncodeOpts.builder

let encode_cmd =
  let open Docs.EncodeCmd in
  let info = Cmd.(info "encode" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(const Cmd_encode.run $ copts $ encode_opts) in
  Cmd.v info term

let execute_opts =
  let open Term in
  const Cmd_execute.Options.set
  $ Docs.FileOpts.input
  $ Docs.ExecuteOpts.lang
  $ Docs.ExecuteOpts.jsinterp
  $ Docs.ExecuteOpts.harness
  $ interpreter_config

let execute_cmd =
  let open Docs.ExecuteCmd in
  let info = Cmd.(info "execute" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(const Cmd_execute.run $ copts $ execute_opts) in
  Cmd.v info term

let test_opts =
  let open Term in
  const Cmd_test.Options.set
  $ Docs.FileOpts.inputs
  $ Docs.ExecuteOpts.lang
  $ Docs.ExecuteOpts.jsinterp
  $ Docs.ExecuteOpts.harness
  $ Docs.TestOpts.test_type
  $ Docs.TestOpts.report
  $ Docs.InterpretOpts.profiler
  $ Docs.TestOpts.webhook_url
  $ Docs.TestOpts.jobs

let test_cmd =
  let open Docs.TestCmd in
  let info = Cmd.(info "test" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(const Cmd_test.run $ copts $ test_opts) in
  Cmd.v info term

let symbolic_opts =
  let open Term in
  const Cmd_symbolic.Options.set
  $ Docs.FileOpts.input
  $ Docs.SymbolicOpts.lang
  $ Docs.SymbolicOpts.target
  $ Docs.SymbolicOpts.workspace

let symbolic_cmd =
  let open Docs.SymbolicCmd in
  let info = Cmd.(info "symbolic" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(const Cmd_symbolic.run $ copts $ symbolic_opts) in
  Cmd.v info term

let cmd_list =
  [ compile_cmd
  ; interpret_cmd
  ; encode_cmd
  ; execute_cmd
  ; test_cmd
  ; symbolic_cmd
  ]

let main_cmd =
  let open Docs.Application in
  let default_f _ = `Help (`Pager, None) in
  let default = Term.(ret (const default_f $ copts)) in
  let info = Cmd.info "ecma-sl" ~sdocs ~doc ~version ~man ~man_xrefs ~exits in
  Cmd.group info ~default cmd_list

let exit_code () =
  match Cmdliner.Cmd.eval_value main_cmd with
  | Ok (`Help | `Version) -> Docs.ExitCodes.ok
  | Ok (`Ok (Ok ())) -> Docs.ExitCodes.ok
  | Ok (`Ok (Error err)) -> begin
    match err with
    | `Compile _ -> Docs.ExitCodes.compile
    | `Runtime _ -> Docs.ExitCodes.interpret
    | `Typing -> Docs.ExitCodes.typing
    | `Encode _ -> Docs.ExitCodes.encode
    | `Execute _ -> Docs.ExitCodes.execute
    | `Test -> Docs.ExitCodes.test
    | `TestFmt _ -> Docs.ExitCodes.test
    | `SymAbort _ -> Docs.ExitCodes.sym_abort
    | `SymAssertFailure _ -> Docs.ExitCodes.sym_assert_failure
    | `SymFailure _ -> Docs.ExitCodes.sym_failure
    | `Generic _ -> Docs.ExitCodes.generic
  end
  | Error err -> begin
    match err with
    | `Term -> Docs.ExitCodes.term
    | `Parse -> Docs.ExitCodes.client
    | `Exn -> Docs.ExitCodes.internal
  end

let () = exit (exit_code ())
