open Ecma_sl

let run_type_checker (untyped : bool) (prog : E_Prog.t) : E_Prog.t =
  if untyped then prog
  else
    let terrs = T_Checker.type_program prog in
    if terrs = [] then prog
    else (
      Printf.eprintf "%s" (T_Checker.terrs_str terrs);
      raise (Cmd.CmdError Cmd.Error) )

let run_compiler (untyped : bool) (input_file : string) : Prog.t =
  Io.load_file input_file
  |> Parsing_utils.parse_e_prog input_file
  |> Parsing_utils.resolve_prog_imports
  |> Parsing_utils.apply_prog_macros
  |> run_type_checker untyped
  |> Compiler.compile_prog

let run (input_file : string) (output_file : string option) (untyped : bool) :
  unit =
  Cmd.test_file_ext input_file [ ".esl" ];
  let prog = run_compiler untyped input_file in
  match output_file with
  | None -> print_endline (Prog.str prog)
  | Some output_file' -> Io.write_file ~file:output_file' ~data:(Prog.str prog)

let main (debug : bool) (input_file : string) (output_file : string option)
  (untyped : bool) : int =
  Log.on_debug := debug;
  Config.file := input_file;
  let run' () = run input_file output_file untyped in
  Cmd.eval_cmd run'
