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

let type_check (prog : EProg.t) : EProg.t =
  if !Options.untyped then prog else prog
(* Uncomment once the type system is working properly *)
(* else *)
(* let terrs = T_Checker.type_program prog in *)
(* if terrs = [] then prog *)
(* else ( *)
(* Fmt.eprintf "%s" (T_Checker.terrs_str terrs); *)
(* raise (Cmd.Command_error Cmd.Error) ) *)

let compile_pipeline (file : string) (path : string) : Prog.t =
  EParsing.load_file ~file path
  |> EParsing.parse_eprog ~file path
  |> Preprocessor.Imports.resolve_imports
  |> Preprocessor.Macros.apply_macros
  |> type_check
  |> Compiler.compile_prog

let compile (input : Fpath.t) : Prog.t =
  let (file, path) = (Fpath.filename input, Fpath.to_string input) in
  let prog = compile_pipeline file path in
  Log.debug "Sucessfuly compiled program '%a'." Fpath.pp input;
  prog

let run (opts : Options.t) : unit =
  ignore Lang.(resolve_file_lang [ ESL ] opts.input);
  let prog = compile opts.input in
  match opts.output with
  | None -> print_endline (Prog.str prog)
  | Some output -> Io.write_file (Fpath.to_string output) (Prog.str prog)

let main () (opts : Options.t) : int = Cmd.eval_cmd (fun () -> run opts)
