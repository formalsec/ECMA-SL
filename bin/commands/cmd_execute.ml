open EslCore
open Ecma_sl

type options =
  { input : Fpath.t
  ; harness : Fpath.t option
  ; lang : Enums.Lang.t
  ; ecmaref : Enums.ECMARef.t
  ; verbose : bool
  ; verbose_at : bool
  ; debugger : bool
  ; show_exitval : bool
  }

let langs : Enums.Lang.t list = Enums.Lang.[ Auto; JS; CESL ]

let compile_interp (ecmaref : Enums.ECMARef.t) : Prog.t =
  let interp_path = Enums.ECMARef.interp ecmaref in
  Cmd_compile.compile (Fpath.v interp_path)

let parse_ast (file : string) : Func.t =
  Parsing_utils.load_file file |> Parsing_utils.parse_func ~file

let execute_partial (config : Interpreter.Config.t) (interp : Prog.t)
  (input : Fpath.t) : Val.t * Val.t Heap.t =
  let ast = parse_ast (Fpath.to_string input) in
  Hashtbl.replace (Prog.funcs interp) (Func.name' ast) ast;
  Cmd_interpret.interpret_partial config interp

let setup_harness (interp : Prog.t) (harness : Fpath.t) : Val.t Heap.t =
  ignore Enums.Lang.(resolve_file_lang [ JS ] harness);
  let ast = Fpath.v (Filename.temp_file "ecmasl" "harness.cesl") in
  Cmd_encode.encode None harness (Some ast);
  let heap = snd (execute_partial Interpreter.Config.default interp ast) in
  Log.debug "Sucessfuly linked JS harness '%a' to interpreter." Fpath.pp harness;
  heap

let setup_execution (ecmaref : Enums.ECMARef.t) (harness : Fpath.t option) :
  Prog.t * Val.t Heap.t option =
  let interp = compile_interp ecmaref in
  let static_heap = Option.map (setup_harness interp) harness in
  (interp, static_heap)

let execute_js ((interp, static_heap) : Prog.t * Val.t Heap.t option)
  (input : Fpath.t) : Val.t =
  let ast = Fpath.v (Filename.temp_file "ecmasl" "ast.js") in
  Cmd_encode.encode None input (Some ast);
  let main = if Option.is_some static_heap then "mainPartial" else "main" in
  let config = { Interpreter.Config.default with main; static_heap } in
  let retval = fst (execute_partial config interp ast) in
  Log.debug "Sucessfuly evaluated program with return '%a'." Val.pp retval;
  retval

let run (opts : options) : unit =
  let valid_langs = Enums.Lang.valid_langs langs opts.lang in
  let setup = setup_execution opts.ecmaref opts.harness in
  Cmd_interpret.process_exitval opts.show_exitval
  @@
  match Enums.Lang.resolve_file_lang valid_langs opts.input with
  | Some JS -> execute_js setup opts.input
  (* FIXME: Allow the execution of pre-encoded js files *)
  (* | Some CESL -> execute_cesl opts.input opts.ecmaref *)
  | _ -> execute_js setup opts.input

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Config.Interpreter.verbose := opts.verbose;
  Config.Interpreter.verbose_at := opts.verbose_at;
  Config.Interpreter.debugger := opts.debugger;
  Cmd.eval_cmd (fun () -> run opts)
