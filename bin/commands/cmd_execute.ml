open Ecma_sl

module Options = struct
  let langs : Enums.Lang.t list = Enums.Lang.[ Auto; JS ]

  type t =
    { input : Fpath.t
    ; harness : Fpath.t option
    ; lang : Enums.Lang.t
    ; ecmaref : Enums.JSInterp.t
    ; show_exitval : bool
    }

  let set input lang ecmaref harness trace trace_loc trace_depth debugger
    show_exitval =
    Cmd_compile.Options.untyped := true;
    Cmd_interpret.Options.trace := trace;
    Cmd_interpret.Options.trace_loc := trace_loc;
    Cmd_interpret.Options.trace_depth := trace_depth;
    Cmd_interpret.Options.debugger := debugger;
    { input; harness; lang; ecmaref; show_exitval }
end

let execute_partial (entry : Interpreter.EntryPoint.t) (interp : Prog.t)
  (input : Fpath.t) : Val.t * Val.t Heap.t =
  let file = Fpath.to_string input in
  let ast = Parsing.load_file file |> Parsing.parse_func ~file in
  Hashtbl.replace (Prog.funcs interp) (Func.name' ast) ast;
  Cmd_interpret.interpret_partial entry interp

let setup_harness (interp : Prog.t) (harness : Fpath.t) : Val.t Heap.t =
  ignore Enums.Lang.(resolve_file_lang [ JS ] harness);
  let ast = Fpath.v (Filename.temp_file "ecmasl" "harness.cesl") in
  Cmd_encode.encode None harness (Some ast);
  let heap = snd (execute_partial Interpreter.EntryPoint.default interp ast) in
  Log.debug "Sucessfuly linked JS harness '%a' to interpreter." Fpath.pp harness;
  heap

let setup_execution (ecmaref : Enums.JSInterp.t) (harness : Fpath.t option) :
  Prog.t * Val.t Heap.t option =
  let finterp = Enums.JSInterp.interp ecmaref in
  let interp = Cmd_compile.compile (Fpath.v finterp) in
  let static_heap = Option.map (setup_harness interp) harness in
  (interp, static_heap)

let execute_js ((interp, static_heap) : Prog.t * Val.t Heap.t option)
  (input : Fpath.t) : Val.t =
  let ast = Fpath.v (Filename.temp_file "ecmasl" "ast.js") in
  Cmd_encode.encode None input (Some ast);
  let main = if Option.is_some static_heap then "mainPartial" else "main" in
  let entry = { Interpreter.EntryPoint.default with main; static_heap } in
  let retval = fst (execute_partial entry interp ast) in
  Log.debug "Sucessfuly evaluated program with return '%a'." Val.pp retval;
  retval

let run (opts : Options.t) : unit =
  let valid_langs = Enums.Lang.valid_langs Options.langs opts.lang in
  let setup = setup_execution opts.ecmaref opts.harness in
  Cmd_interpret.process_exitval opts.show_exitval
  @@
  match Enums.Lang.resolve_file_lang valid_langs opts.input with
  | Some JS -> execute_js setup opts.input
  | _ -> execute_js setup opts.input

let main () (opts : Options.t) : int = Cmd.eval_cmd (fun () -> run opts)
