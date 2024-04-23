open Ecma_sl

module Options = struct
  let langs : Enums.Lang.t list = Enums.Lang.[ Auto; JS ]

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

let execute_partial (entry : Interpreter.EntryPoint.t)
  (instrument : Options.instrument) (interp : Prog.t) (input : Fpath.t) :
  Val.t * Val.t Heap.t =
  let file = Fpath.to_string input in
  let ast = Parsing.load_file file |> Parsing.parse_func ~file in
  Hashtbl.replace (Prog.funcs interp) (Func.name' ast) ast;
  Cmd_interpret.interpret_partial entry instrument interp

let setup_harness (instrument : Options.instrument) (interp : Prog.t)
  (harness : Fpath.t) : Val.t Heap.t =
  ignore Enums.Lang.(resolve_file_lang [ JS ] harness);
  let ast = Fpath.v (Filename.temp_file "ecmasl" "harness.cesl") in
  Cmd_encode.encode None harness (Some ast);
  let entry = Interpreter.EntryPoint.default in
  let heap = snd (execute_partial entry instrument interp ast) in
  Log.debug "Sucessfuly linked JS harness '%a' to interpreter." Fpath.pp harness;
  heap

let setup_execution (jsinterp : Enums.JSInterp.t) (harness : Fpath.t option)
  (instrument : Options.instrument) : Prog.t * Val.t Heap.t option =
  let finterp = Enums.JSInterp.interp jsinterp in
  let interp = Cmd_compile.compile true (Fpath.v finterp) in
  let static_heap = Option.map (setup_harness instrument interp) harness in
  (interp, static_heap)

let execute_js ((interp, static_heap) : Prog.t * Val.t Heap.t option)
  (instrument : Options.instrument) (input : Fpath.t) : Val.t =
  let ast = Fpath.v (Filename.temp_file "ecmasl" "ast.js") in
  Cmd_encode.encode None input (Some ast);
  let main = if Option.is_some static_heap then "mainPartial" else "main" in
  let entry = { Interpreter.EntryPoint.default with main; static_heap } in
  let retval = fst (execute_partial entry instrument interp ast) in
  Log.debug "Sucessfuly evaluated program with return '%a'." Val.pp retval;
  retval

let run () (opts : Options.t) : unit =
  let valid_langs = Enums.Lang.valid_langs Options.langs opts.lang in
  let setup = setup_execution opts.jsinterp opts.harness opts.instrument in
  Cmd_interpret.process_exitval opts.show_exitval
  @@
  match Enums.Lang.resolve_file_lang valid_langs opts.input with
  | Some JS -> execute_js setup opts.instrument opts.input
  | _ -> execute_js setup opts.instrument opts.input
