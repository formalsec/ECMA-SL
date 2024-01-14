open Ecma_sl

type options =
  { input_file : string
  ; output_file : string option
  ; untyped : bool
  }

let options input_file output_file untyped : options =
  { input_file; output_file; untyped }

let run_type_checker (prog : EProg.t) : EProg.t =
  if !Config.Tesl.untyped || true then prog
  else
    let terrs = T_Checker.type_program prog in
    if terrs = [] then prog
    else (
      Format.eprintf "%s" (T_Checker.terrs_str terrs);
      raise (Cmd.Command_error Cmd.Error) )

let run_compiler (input_file : string) : Prog.t =
  Io.load_file input_file
  |> Parsing_utils.parse_e_prog input_file
  |> Parsing_utils.resolve_prog_imports
  |> Parsing_utils.apply_prog_macros
  |> run_type_checker
  |> Compiler.compile_prog

let run (opts : options) : unit =
  ignore (Cmd.test_file_lang opts.input_file [ Lang.ESL ]);
  let prog = run_compiler opts.input_file in
  match opts.output_file with
  | None -> print_endline (Prog.str prog)
  | Some output_file' -> Io.write_file ~file:output_file' ~data:(Prog.str prog)

let main (copts : Options.common_options) (opts : options) : int =
  Log.on_debug := copts.debug;
  Config.Common.colored := not copts.colorless;
  Config.Tesl.untyped := opts.untyped;
  Config.file := opts.input_file;
  Cmd.eval_cmd (fun () -> run opts)
