open Ecma_sl
open Smtml.Syntax.Result

module Options = struct
  let langs : Enums.Lang.t list = Enums.Lang.[ Auto; JS; CESL ]

  type interp_config = Cmd_interpret.Options.config

  type t =
    { input : Fpath.t
    ; lang : Enums.Lang.t
    ; jsinterp : Enums.JSInterp.t
    ; harness : Fpath.t option
    ; interp_config : Cmd_interpret.Options.config
    }

  let set (input : Fpath.t) (lang : Enums.Lang.t) (jsinterp : Enums.JSInterp.t)
    (harness : Fpath.t option) (interp_config : interp_config) =
    { input; lang; jsinterp; harness; interp_config }
end

let build_ast (file : Fpath.t) : Func.t Result.t =
  Result.esl_exec @@ fun () ->
  let fpath = Fpath.to_string file in
  let p = Parsing.load_file fpath |> Parsing.parse_func ~file:fpath in
  Log.debug "Sucessfuly loaded AST builder '%a'." Fpath.pp file;
  Ok p

let execute_partial (entry : Interpreter.IEntry.t)
  (config : Options.interp_config) (interp : Prog.t) (ast : Fpath.t) :
  Interpreter.IResult.t Result.t =
  Result.esl_exec @@ fun () ->
  let* build_ast = build_ast ast in
  Hashtbl.replace (Prog.funcs interp) (Func.name' build_ast) build_ast;
  Ok (Cmd_interpret.interpret_partial entry config interp)

let check_harness_return (result : Interpreter.IResult.t) : unit Result.t =
  match result.retval with
  | App (`Op "loc", [ Int loc ]) -> (
    let* (type_, _, _) = Interpreter.IResult.get_completion result.heap loc in
    match type_ with
    | App (`Op "symbol", [ Str "normal" ]) -> Ok ()
    | _ -> Result.error (`Execute "Harness return non-normal completion") )
  | _ ->
    let err = Fmt.str "Unable to setup harness: %a" Value.pp result.retval in
    Result.error (`Execute err)

let setup_program_harness (interp : Prog.t) (harness : Fpath.t) :
  Value.t Heap.t Result.t =
  ignore Enums.Lang.(resolve_file_lang [ JS ] harness);
  let ast = Fpath.v (Filename.temp_file "ecmasl" "harness.cesl") in
  let entry = Interpreter.IEntry.default () in
  let config = Cmd_interpret.Options.default_config () in
  let* () = Cmd_encode.encode None harness (Some ast) in
  let* result = execute_partial entry config interp ast in
  let* () = check_harness_return result in
  Log.debug "Sucessfuly linked JS harness '%a' to interpreter." Fpath.pp harness;
  Ok result.heap

let setup_execution (jsinterp : Enums.JSInterp.t) (harness : Fpath.t option) :
  (Prog.t * Value.t Heap.t option) Result.t =
  let finterp = Enums.JSInterp.interp jsinterp in
  let* interp = Cmd_compile.compile true (Fpath.v finterp) in
  match harness with
  | None -> Ok (interp, None)
  | Some harness' ->
    let* static_heap = setup_program_harness interp harness' in
    Ok (interp, Some static_heap)

let execute_cesl ((interp, heap) : Prog.t * Value.t Heap.t option)
  (config : Options.interp_config) (input : Fpath.t) :
  Interpreter.IResult.t Result.t =
  let pre_initialized = Option.is_some heap in
  let main = if pre_initialized then "mainPreInitialized" else "main" in
  let entry = Interpreter.IEntry.{ main; heap } in
  let* result = execute_partial entry config interp input in
  let retval = result.retval in
  Log.debug "Sucessfuly evaluated program with return '%a'." Value.pp retval;
  Ok result

let execute_js (setup : Prog.t * Value.t Heap.t option)
  (config : Options.interp_config) (input : Fpath.t) :
  Interpreter.IResult.t Result.t =
  let ast = Fpath.v (Filename.temp_file "ecmasl" "ast.cesl") in
  let* () = Cmd_encode.encode None input (Some ast) in
  execute_cesl setup config ast

let run () (opts : Options.t) : unit Result.t =
  let valid_langs = Enums.Lang.valid_langs Options.langs opts.lang in
  let* setup = setup_execution opts.jsinterp opts.harness in
  Cmd_interpret.log_metrics opts.interp_config.instrument.profiler
  @@
  match Enums.Lang.resolve_file_lang valid_langs opts.input with
  | Some JS -> execute_js setup opts.interp_config opts.input
  | Some CESL -> execute_cesl setup opts.interp_config opts.input
  | _ -> execute_js setup opts.interp_config opts.input
