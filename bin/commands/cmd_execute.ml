open Ecma_sl
open Syntax.Result

module Options = struct
  let langs : Enums.Lang.t list = Enums.Lang.[ Auto; JS; CESL ]

  type instrument = Cmd_interpret.Options.instrument

  type t =
    { input : Fpath.t
    ; lang : Enums.Lang.t
    ; jsinterp : Enums.JSInterp.t
    ; harness : Fpath.t option
    ; instrument : instrument
    ; show_exitval : bool
    }

  let set (input : Fpath.t) (lang : Enums.Lang.t) (jsinterp : Enums.JSInterp.t)
    (harness : Fpath.t option) (tr_mode : Enums.InterpTracer.t) (tr_loc : bool)
    (tr_depth : int) (debugger : bool) (show_exitval : bool) =
    let open Cmd_interpret.Options in
    let tracer = { mode = tr_mode; loc = tr_loc; depth = tr_depth } in
    let instrument = { tracer; debugger } in
    { input; lang; jsinterp; harness; instrument; show_exitval }
end

let build_ast (file : Fpath.t) : Func.t Result.t =
  Result.esl_exec @@ fun () ->
  let fpath = Fpath.to_string file in
  let p = Parsing.load_file fpath |> Parsing.parse_func ~file:fpath in
  Log.debug "Sucessfuly loaded AST builder '%a'." Fpath.pp file;
  Ok p

let execute_partial (entry : Interpreter.EntryPoint.t)
  (instrument : Options.instrument) (interp : Prog.t) (ast : Fpath.t) :
  (Val.t * Val.t Heap.t) Result.t =
  let* build_ast = build_ast ast in
  Hashtbl.replace (Prog.funcs interp) (Func.name' build_ast) build_ast;
  Ok (Cmd_interpret.interpret_partial entry instrument interp)

let setup_harness (instrument : Options.instrument) (interp : Prog.t)
  (harness : Fpath.t) : Val.t Heap.t Result.t =
  ignore Enums.Lang.(resolve_file_lang [ JS ] harness);
  let ast = Fpath.v (Filename.temp_file "ecmasl" "harness.cesl") in
  let entry = Interpreter.EntryPoint.default in
  let* () = Cmd_encode.encode None harness (Some ast) in
  let* (_, static_heap) = execute_partial entry instrument interp ast in
  Log.debug "Sucessfuly linked JS harness '%a' to interpreter." Fpath.pp harness;
  Ok static_heap

let setup_execution (jsinterp : Enums.JSInterp.t) (harness : Fpath.t option)
  (instrument : Options.instrument) : (Prog.t * Val.t Heap.t option) Result.t =
  let finterp = Enums.JSInterp.interp jsinterp in
  let* interp = Cmd_compile.compile true (Fpath.v finterp) in
  match harness with
  | None -> Ok (interp, None)
  | Some harness' ->
    let* static_heap = setup_harness instrument interp harness' in
    Ok (interp, Some static_heap)

let execute_cesl ((interp, static_heap) : Prog.t * Val.t Heap.t option)
  (instrument : Options.instrument) (input : Fpath.t) : Val.t Result.t =
  let main = if Option.is_some static_heap then "mainPartial" else "main" in
  let entry = { Interpreter.EntryPoint.default with main; static_heap } in
  let* (retval, _) = execute_partial entry instrument interp input in
  Log.debug "Sucessfuly evaluated program with return '%a'." Val.pp retval;
  Ok retval

let execute_js (setup : Prog.t * Val.t Heap.t option)
  (instrument : Options.instrument) (input : Fpath.t) : Val.t Result.t =
  let ast = Fpath.v (Filename.temp_file "ecmasl" "ast.cesl") in
  let* () = Cmd_encode.encode None input (Some ast) in
  execute_cesl setup instrument ast

let run_executer (opts : Options.t) : Val.t Result.t =
  let valid_langs = Enums.Lang.valid_langs Options.langs opts.lang in
  let* setup = setup_execution opts.jsinterp opts.harness opts.instrument in
  match Enums.Lang.resolve_file_lang valid_langs opts.input with
  | Some JS -> execute_js setup opts.instrument opts.input
  | Some CESL -> execute_cesl setup opts.instrument opts.input
  | _ -> execute_js setup opts.instrument opts.input

let run () (opts : Options.t) : unit Result.t =
  let* exitval = run_executer opts in
  Cmd_interpret.show_exitval opts.show_exitval exitval
