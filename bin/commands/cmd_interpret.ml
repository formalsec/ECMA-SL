open Ecma_sl

type options =
  { input_file : string
  ; interpret_esl : bool
  ; interpret_verbose : bool
  ; interpret_debugger : bool
  ; untyped : bool
  }

let options input_file interpret_esl interpret_verbose interpret_debugger
  untyped : options =
  { input_file; interpret_esl; interpret_verbose; interpret_debugger; untyped }

let configure_debugger () : (module Debugger.M) =
  match !Config.Interpreter.debugger with
  | true -> (module Debugger.Enable : Debugger.M)
  | false -> (module Debugger.Disable : Debugger.M)

let configure_verbose () : (module Verbose.M) =
  match !Config.Interpreter.verbose with
  | true -> (module Verbose.Enable : Verbose.M)
  | false -> (module Verbose.Disable : Verbose.M)

let configure_monitor () : (module Monitor.M) =
  (module Monitor.Default : Monitor.M)

let run_interpreter (prog : Prog.t) : unit =
  let module Debugger = (val configure_debugger ()) in
  let module Verbose = (val configure_verbose ()) in
  let module Monitor = (val configure_monitor ()) in
  let module Interpreter = Interpreter.M (Debugger) (Verbose) (Monitor) in
  ignore (Interpreter.eval_prog prog)

let interpret_core (input_file : string) : unit =
  Cmd.test_file_ext input_file [ ".cesl" ];
  Io.load_file input_file |> Parsing_utils.parse_prog |> run_interpreter

let compile_and_interpret (input_file : string) : unit =
  Cmd.test_file_ext input_file [ ".esl" ];
  Cmd_compile.run_compiler input_file |> run_interpreter

let run (opts : options) : unit =
  match opts.interpret_esl with
  | true -> compile_and_interpret opts.input_file
  | false -> interpret_core opts.input_file

let main (copts : Options.common_options) (opts : options) : int =
  Log.on_debug := copts.debug;
  Config.Common.colored := not copts.colorless;
  Config.Eslerr.show_code := not opts.interpret_esl;
  Config.Interpreter.verbose := opts.interpret_verbose;
  Config.Interpreter.debugger := opts.interpret_debugger;
  Config.Tesl.untyped := opts.untyped;
  Config.file := opts.input_file;
  Cmd.eval_cmd (fun () -> run opts)
