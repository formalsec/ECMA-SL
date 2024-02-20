open Ecma_sl

type options =
  { input : Fpath.t
  ; output : Fpath.t option
  ; untyped : bool
  }

let type_check (prog : EProg.t) : EProg.t =
  if !Config.Tesl.untyped || true then prog
  else
    let terrs = T_Checker.type_program prog in
    if terrs = [] then prog
    else (
      Fmt.eprintf "%s" (T_Checker.terrs_str terrs);
      raise (Cmd.Command_error Cmd.Error) )

let compile (input : Fpath.t) : Prog.t =
  let input' = Fpath.to_string input in
  Parsing_utils.load_file input'
  |> Parsing_utils.parse_eprog ~file:input'
  |> Parsing_utils.resolve_eprog_imports
  |> Parsing_utils.apply_eprog_macros
  |> type_check
  |> Compiler.compile_prog

let run (opts : options) : unit =
  ignore Enums.Lang.(resolve_file_lang [ ESL ] opts.input);
  let prog = compile opts.input in
  match opts.output with
  | None -> print_endline (Prog.str prog)
  | Some output_file' ->
    Io.write_file (Fpath.to_string output_file') (Prog.str prog)

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Config.Tesl.untyped := opts.untyped;
  Cmd.eval_cmd (fun () -> run opts)
