open Ecma_sl

module Options = struct
  type t =
    { input : Fpath.t
    ; output : Fpath.t option
    ; untyped : bool
    }

  let set (input : Fpath.t) (output : Fpath.t option) (untyped : bool) : t =
    { input; output; untyped }
end

let type_check (untyped : bool) (p : EProg.t) : EProg.t =
  if untyped then p
  else if TChecker.type_prog p then p
  else raise (Exec.Command_error Exec.CompileError)

let compile_pipeline (untyped : bool) (file : string) (path : string) : Prog.t =
  EParsing.load_file ~file path
  |> EParsing.parse_eprog ~file path
  |> Preprocessor.Imports.resolve_imports ~stdlib:Share.stdlib
  |> Preprocessor.Macros.apply_macros
  |> type_check untyped
  |> Compiler.compile_prog

let compile (untyped : bool) (file : Fpath.t) : Prog.t =
  let (fname, path) = (Fpath.filename file, Fpath.to_string file) in
  let prog = compile_pipeline untyped fname path in
  Log.debug "Sucessfuly compiled program '%a'." Fpath.pp file;
  prog

let run () (opts : Options.t) : unit =
  ignore Enums.Lang.(resolve_file_lang [ ESL ] opts.input);
  let prog = compile opts.untyped opts.input in
  match opts.output with
  | None -> Log.out "%a@." Prog.pp prog
  | Some output -> Io.write_file (Fpath.to_string output) (Prog.str prog)
