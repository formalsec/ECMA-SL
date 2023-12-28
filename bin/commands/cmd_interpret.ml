open Ecma_sl
module Interpreter = Interpreter.M (Monitor.Default)

let run_interpreter (_heap_file : string option) (prog : Prog.t) : unit =
  ignore (Interpreter.eval_prog prog)

let interpret_core (input_file : string) (heap_file : string option) : unit =
  Cmd.test_file_ext input_file [ ".cesl" ];
  Io.load_file input_file
  |> Parsing_utils.parse_prog
  |> run_interpreter heap_file

let compile_and_interpret (input_file : string) (heap_file : string option)
  (untyped : bool) : unit =
  Config.Eslerr.show_code := false;
  Cmd.test_file_ext input_file [ ".esl" ];
  Cmd_compile.run_compiler untyped input_file |> run_interpreter heap_file

let run (input_file : string) (heap_file : string option) (interpret_esl : bool)
  (untyped : bool) : unit =
  match interpret_esl with
  | true -> compile_and_interpret input_file heap_file untyped
  | false -> interpret_core input_file heap_file

let main (debug : bool) (input_file : string) (heap_file : string option)
  (interpret_esl : bool) (untyped : bool) : int =
  Log.on_debug := debug;
  Config.file := input_file;
  let run' () = run input_file heap_file interpret_esl untyped in
  Cmd.eval_cmd run'
