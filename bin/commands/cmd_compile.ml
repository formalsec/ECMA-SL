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

let run_compiler (file : string) : Prog.t =
  Parsing_utils.load_file file
  |> Parsing_utils.parse_eprog ~file
  |> Parsing_utils.resolve_eprog_imports
  |> Parsing_utils.apply_eprog_macros
  |> run_type_checker
  |> Compiler.compile_prog

let run (opts : options) : unit =
  ignore (Cmd.test_file_lang opts.input_file [ Lang.ESL ]);
  let prog = run_compiler opts.input_file in
  match opts.output_file with
  | None -> print_endline (Prog.str prog)
  | Some output_file' -> Io.write_file output_file' (Prog.str prog)

let main (copts : Options.common_options) (opts : options) : int =
  Log.on_debug := copts.debug;
  Config.Common.colored := not copts.colorless;
  Config.Tesl.untyped := opts.untyped;
  Cmd.eval_cmd (fun () -> run opts)
