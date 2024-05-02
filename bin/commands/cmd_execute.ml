open Ecma_sl
open Syntax.Result

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

let execute_partial (entry : Interpreter.EntryPoint.t)
  (config : Options.interp_config) (interp : Prog.t) (ast : Fpath.t) :
  (Val.t * Val.t Heap.t) Result.t =
  let* build_ast = build_ast ast in
  Hashtbl.replace (Prog.funcs interp) (Func.name' build_ast) build_ast;
  Ok (Cmd_interpret.interpret_partial entry config interp)

let setup_harness (config : Options.interp_config) (interp : Prog.t)
  (harness : Fpath.t) : Val.t Heap.t Result.t =
  ignore Enums.Lang.(resolve_file_lang [ JS ] harness);
  let ast = Fpath.v (Filename.temp_file "ecmasl" "harness.cesl") in
  let entry = Interpreter.EntryPoint.default in
  let* () = Cmd_encode.encode None harness (Some ast) in
  let* (_, static_heap) = execute_partial entry config interp ast in
  Log.debug "Sucessfuly linked JS harness '%a' to interpreter." Fpath.pp harness;
  Ok static_heap

let setup_execution (jsinterp : Enums.JSInterp.t) (harness : Fpath.t option)
  (config : Options.interp_config) : (Prog.t * Val.t Heap.t option) Result.t =
  let finterp = Enums.JSInterp.interp jsinterp in
  let* interp = Cmd_compile.compile true (Fpath.v finterp) in
  match harness with
  | None -> Ok (interp, None)
  | Some harness' ->
    let* static_heap = setup_harness config interp harness' in
    Ok (interp, Some static_heap)

let execute_cesl ((interp, static_heap) : Prog.t * Val.t Heap.t option)
  (config : Options.interp_config) (input : Fpath.t) : Val.t Result.t =
  let main = if Option.is_some static_heap then "mainPartial" else "main" in
  let entry = Interpreter.EntryPoint.{ main; static_heap } in
  let* (retval, _) = execute_partial entry config interp input in
  Log.debug "Sucessfuly evaluated program with return '%a'." Val.pp retval;
  Ok retval

let execute_js (setup : Prog.t * Val.t Heap.t option)
  (config : Options.interp_config) (input : Fpath.t) : Val.t Result.t =
  let ast = Fpath.v (Filename.temp_file "ecmasl" "ast.cesl") in
  let* () = Cmd_encode.encode None input (Some ast) in
  execute_cesl setup config ast

let run () (opts : Options.t) : unit Result.t =
  let valid_langs = Enums.Lang.valid_langs Options.langs opts.lang in
  let* setup = setup_execution opts.jsinterp opts.harness opts.interp_config in
  Syntax.Result.map ignore
  @@
  match Enums.Lang.resolve_file_lang valid_langs opts.input with
  | Some JS -> execute_js setup opts.interp_config opts.input
  | Some CESL -> execute_cesl setup opts.interp_config opts.input
  | _ -> execute_js setup opts.interp_config opts.input
