open Ecma_sl
module Interpreter = Interpreter.M (Monitor.Default)

let run_interpreter (_heap_file : string option) (prog : Prog.t) : unit =
  let _output_value value =
    match value with
    | Val.Tuple [ error; completion ] -> (
      print_string ("\nMAIN error -> " ^ Val.str error ^ "\n");
      print_string ("MAIN return -> " ^ Val.str completion ^ "\n");
      match completion with
      | Val.Tuple [ _; Val.Symbol "normal" ] -> ()
      | _ -> raise (Cmd.Command_error Cmd.Failure) )
    | Val.Str s when String.sub s 0 11 = "Unsuported" ->
      print_string ("\nMAIN fail -> " ^ s);
      raise (Cmd.Command_error Cmd.Unsupported)
    | v -> print_string ("\nMAIN return -> " ^ Val.str v)
  in
  let value = Interpreter.eval_prog prog in
  _output_value value

let interpret_core (input_file : string) (heap_file : string option) : unit =
  Cmd.test_file_ext input_file [ ".cesl" ];
  Io.load_file input_file
  |> Parsing_utils.parse_prog
  |> run_interpreter heap_file

let compile_and_interpret (input_file : string) (heap_file : string option)
  (untyped : bool) : unit =
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
