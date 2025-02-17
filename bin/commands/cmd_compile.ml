open Ecma_sl
open Smtml_prelude.Result

module Options = struct
  type t =
    { input : Fpath.t
    ; output : Fpath.t option
    ; untyped : bool
    ; advices : Fpath.t list
    }

  let set (input : Fpath.t) (output : Fpath.t option) (untyped : bool)
    (advices : Fpath.t list) : t =
    { input; output; untyped; advices }
end

let type_check (untyped : bool) (p : EProg.t) : EProg.t Result.t =
  if untyped then Ok p else if TChecker.type_prog p then Ok p else Error `Typing

let compile_pipeline (untyped : bool) (advices : Fpath.t list) (fname : string)
  (fpath : string) : Prog.t Result.t =
  EParsing.load_file ~file:fname fpath
  |> EParsing.parse_eprog ~file:fname fpath
  |> Preprocessor.Advices.add_advices advices
  |> Preprocessor.Imports.resolve_imports ~stdlib:Share.stdlib
  |> Preprocessor.Macros.apply_macros
  |> type_check untyped
  |> map Compiler.compile_prog

let load (file : Fpath.t) : Prog.t Result.t =
  Result.esl_exec @@ fun () ->
  let fpath = Fpath.to_string file in
  let p = Parsing.load_file fpath |> Parsing.parse_prog ~file:fpath in
  Log.debug "Sucessfuly loaded program '%a'." Fpath.pp file;
  Ok p

let compile (untyped : bool) (advices : Fpath.t list) (file : Fpath.t) :
  Prog.t Result.t =
  Result.esl_exec @@ fun () ->
  let (fname, fpath) = (Fpath.filename file, Fpath.to_string file) in
  let* p = compile_pipeline untyped advices fname fpath in
  Log.debug "Sucessfuly compiled program '%a'." Fpath.pp file;
  Ok p

let run () (opts : Options.t) : unit Result.t =
  ignore Enums.Lang.(resolve_file_lang [ ESL ] opts.input);
  let* p = compile opts.untyped opts.advices opts.input in
  match opts.output with
  | None -> Ok (Log.stdout "%a@." Prog.pp p)
  | Some output' -> Result.bos (Bos.OS.File.writef output' "%a" Prog.pp p)
