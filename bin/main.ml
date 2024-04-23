open Cmdliner

let cmd (cmd_fun : unit -> 'a -> unit) : (unit -> 'a -> int) Term.t =
  Term.const (Exec.eval_cmd cmd_fun)

let set_copts (debug : Enums.DebugLvl.t) (colorless : bool) : unit =
  let open Ecma_sl in
  Log.Config.warns := debug >= Warn;
  Log.Config.debugs := debug >= Full;
  Font.Config.colored := not colorless

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
  let term = Term.(cmd Cmd_compile.run $ copts $ compile_opts) in
  Cmd.v info term

let interpret_opts =
  let open Term in
  const Cmd_interpret.Options.set
  $ Docs.FileOpts.input
  $ Docs.InterpretOpts.lang
  $ Docs.InterpretOpts.tracer
  $ Docs.InterpretOpts.tracer_loc
  $ Docs.InterpretOpts.tracer_depth
  $ Docs.InterpretOpts.debugger
  $ Docs.InterpretOpts.main
  $ Docs.InterpretOpts.show_exitval
  $ Docs.CompileOpts.untyped

let interpret_cmd =
  let open Docs.CompileCmd in
  let info = Cmd.(info "interpret" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(cmd Cmd_interpret.run $ copts $ interpret_opts) in
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
  let term = Term.(cmd Cmd_encode.run $ copts $ encode_opts) in
  Cmd.v info term

let execute_opts =
  let open Term in
  const Cmd_execute.Options.set
  $ Docs.FileOpts.input
  $ Docs.ExecuteOpts.lang
  $ Docs.ExecuteOpts.jsinterp
  $ Docs.ExecuteOpts.harness
  $ Docs.InterpretOpts.tracer
  $ Docs.InterpretOpts.tracer_loc
  $ Docs.InterpretOpts.tracer_depth
  $ Docs.InterpretOpts.debugger
  $ Docs.InterpretOpts.show_exitval

let execute_cmd =
  let open Docs.ExecuteCmd in
  let info = Cmd.(info "execute" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(cmd Cmd_execute.run $ copts $ execute_opts) in
  Cmd.v info term

let test_opts =
  let open Term in
  const Cmd_test.Options.set
  $ Docs.FileOpts.inputs
  $ Docs.ExecuteOpts.lang
  $ Docs.ExecuteOpts.jsinterp
  $ Docs.ExecuteOpts.harness

let test_cmd =
  let open Docs.TestCmd in
  let info = Cmd.(info "test" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(cmd Cmd_test.run $ copts $ test_opts) in
  Cmd.v info term

let symbolic_opts =
  let open Term in
  const Cmd_symbolic.options
  $ Docs.FileOpts.input
  $ Docs.SymbolicOpts.target
  $ Docs.SymbolicOpts.workspace

let symbolic_cmd =
  let open Docs.SymbolicCmd in
  let info = Cmd.(info "symbolic" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(cmd Cmd_symbolic.run $ copts $ symbolic_opts) in
  Cmd.v info term

let replay_opts =
  let open Term in
  const Cmd_replay.options $ Docs.FileOpts.input $ Docs.ReplayOpts.testsuit

let replay_cmd =
  let open Docs.ReplayCmd in
  let info = Cmd.(info "replay" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(cmd Cmd_replay.run $ copts $ replay_opts) in
  Cmd.v info term

let explode_opts = symbolic_opts

let explode_cmd =
  let open Docs.ExplodeJSCmd in
  let info = Cmd.(info "explode-js" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(cmd Cmd_explodejs.run $ copts $ explode_opts) in
  Cmd.v info term

let cmd_list =
  [ compile_cmd
  ; interpret_cmd
  ; encode_cmd
  ; execute_cmd
  ; test_cmd
  ; symbolic_cmd
  ; replay_cmd
  ; explode_cmd
  ]

let main_cmd =
  let open Docs.Application in
  let default_fun _ = `Help (`Pager, None) in
  let default = Term.(ret (const default_fun $ copts)) in
  let info = Cmd.info "ecma-sl" ~sdocs ~doc ~version ~man ~man_xrefs ~exits in
  Cmd.group info ~default cmd_list

let () =
  let open Ecma_sl in
  Printexc.record_backtrace true;
  try exit (Cmdliner.Cmd.eval' main_cmd)
  with exn ->
    flush_all ();
    Log.err "%s: uncaught exception %s@." Sys.argv.(0) (Printexc.to_string exn);
    Printexc.print_backtrace stderr;
    exit Exec.(status_code Failure)
