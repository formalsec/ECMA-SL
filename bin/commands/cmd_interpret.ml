open Ecma_sl
module Interpreter = Interpreter.M (Monitor.Default)

type options =
  { input_file : string
  ; interpret_esl : bool
  ; untyped : bool
  }

let options input_file interpret_esl untyped : options =
  { input_file; interpret_esl; untyped }

let run_interpreter (prog : Prog.t) : unit = ignore (Interpreter.eval_prog prog)

let interpret_core (input_file : string) : unit =
  Cmd.test_file_ext input_file [ ".cesl" ];
  Io.load_file input_file |> Parsing_utils.parse_prog |> run_interpreter

let compile_and_interpret (input_file : string) (untyped : bool) : unit =
  Config.Eslerr.show_code := false;
  Cmd.test_file_ext input_file [ ".esl" ];
  Cmd_compile.run_compiler untyped input_file |> run_interpreter

let run (opts : options) : unit =
  match opts.interpret_esl with
  | true -> compile_and_interpret opts.input_file opts.untyped
  | false -> interpret_core opts.input_file

let main (copts : Options.common_options) (opts : options) : int =
  Log.on_debug := copts.debug;
  Config.Common.colored := (not copts.colorless);
  Config.file := opts.input_file;
  Cmd.eval_cmd (fun () -> run opts)
