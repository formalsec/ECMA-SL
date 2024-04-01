open Ecma_sl

module Options = struct
  let untyped = ref false

  type t =
    { input : Fpath.t
    ; output : Fpath.t option
    }

  let set_options input output untyped' =
    untyped := untyped';
    { input; output }
end

let type_check (p : EProg.t) : EProg.t =
  if !Options.untyped then p
  else if TChecker.type_prog p then p
  else raise (Cmd.Command_error Cmd.CompileError)

let compile_pipeline (file : string) (path : string) : Prog.t =
  EParsing.load_file ~file path
  |> EParsing.parse_eprog ~file path
  |> Preprocessor.Imports.resolve_imports ~stdlib:Share.std_path
  |> Preprocessor.Macros.apply_macros
  |> type_check
  |> Compiler.compile_prog

let compile (file : Fpath.t) : Prog.t =
  let (fname, path) = (Fpath.filename file, Fpath.to_string file) in
  let prog = compile_pipeline fname path in
  Log.debug "Sucessfuly compiled program '%a'." Fpath.pp file;
  prog

let run (opts : Options.t) : unit =
  ignore Lang.(resolve_file_lang [ ESL ] opts.input);
  let prog = compile opts.input in
  match opts.output with
  | None -> print_endline (Prog.str prog)
  | Some output -> Io.write_file (Fpath.to_string output) (Prog.str prog)

let main () (opts : Options.t) : int = Cmd.eval_cmd (fun () -> run opts)
