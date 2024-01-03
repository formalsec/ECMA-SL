open Ecma_sl
module Interpreter = Interpreter.M (Monitor.Default)

type options =
  { input_file : string
  ; interpret_esl : bool
  ; interpret_verbose : bool
  ; untyped : bool
  }

let options input_file interpret_esl interpret_verbose untyped : options =
  { input_file; interpret_esl; interpret_verbose; untyped }

let run_interpreter (prog : Prog.t) : unit = ignore (Interpreter.eval_prog prog)

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
  Config.Tesl.untyped := opts.untyped;
  Config.file := opts.input_file;
  Cmd.eval_cmd (fun () -> run opts)
