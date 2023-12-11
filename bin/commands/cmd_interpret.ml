open Ecma_sl
module NSU = NSU_Monitor.M (SecLevel)
module Interpreter = Interpreter.M (NSU)

let run_interpreter (heap_file : string option) (prog : Prog.t) : unit =
  let _output_heap heap =
    match heap_file with
    | None -> ()
    | Some heap_file' ->
      let data = Heap.to_string_with_glob heap (Val.str ~flt_with_dot:false) in
      Io.write_file ~file:heap_file' ~data
  in
  let _output_value value =
    match value with
    | Some (Val.Tuple [ error; completion ]) -> (
      print_string ("MAIN error -> " ^ Val.str error ^ "\n");
      print_string ("MAIN return -> " ^ Val.str completion ^ "\n");
      match completion with
      | Val.Tuple [ _; Val.Symbol "normal" ] -> ()
      | _ -> raise (Cmd.CmdError Cmd.Failure) )
    | Some (Val.Str s) when String.sub s 0 11 = "Unsuported" ->
      print_string ("MAIN fail -> " ^ s);
      raise (Cmd.CmdError Cmd.Unsupported)
    | Some v -> print_string ("MAIN return -> " ^ Val.str v)
    | _ ->
      Cmd.log "core interpretation error\n";
      raise (Cmd.CmdError Cmd.Error)
  in
  let value, heap = Interpreter.eval_prog prog in
  _output_heap heap;
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

let run (input_file : string) (heap_file : string option) (target_esl : bool)
  (untyped : bool) : unit =
  match target_esl with
  | true -> compile_and_interpret input_file heap_file untyped
  | false -> interpret_core input_file heap_file

let main (debug : bool) (input_file : string) (heap_file : string option)
  (target_esl : bool) (untyped : bool) : int =
  Log.on_debug := debug;
  Config.file := input_file;
  let run' () = run input_file heap_file target_esl untyped in
  Cmd.eval_cmd run'
