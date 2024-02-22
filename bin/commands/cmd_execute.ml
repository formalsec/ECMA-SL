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

let merge_input_harness (input : Fpath.t) (harness : Fpath.t option) : Fpath.t =
  match harness with
  | None -> input
  | Some harness ->
    let input_code = Io.read_file (Fpath.to_string input) in
    let harness_code = Io.read_file (Fpath.to_string harness) in
    let file = Filename.temp_file "ecmasl" "js_wharness.js" in
    Io.write_file file (harness_code ^ input_code);
    Fpath.v file

let execute_cesl (resolve_exitval_f : Val.t -> Val.t) (input : Fpath.t)
  (ecmaref : Enums.ECMARef.t) (show_exitval : bool) : unit =
  let open Ecma_sl in
  let finterp = Enums.ECMARef.interp ecmaref in
  let interp = Parsing_utils.load_file finterp in
  let ast = Parsing_utils.load_file (Fpath.to_string input) in
  String.concat ";\n" [ ast; interp ]
  |> Parsing_utils.parse_prog
  |> Cmd_interpret.interpret "main"
  |> Cmd_interpret.process_exitval resolve_exitval_f show_exitval

let execute_js (resolve_exitval_f : Val.t -> Val.t) (input : Fpath.t)
  (harness : Fpath.t option) (ecmaref : Enums.ECMARef.t) (show_exitval : bool) :
  unit =
  let input' = merge_input_harness input harness in
  let output' = Fpath.v (Filename.temp_file "ecmasl" "ast.js") in
  Cmd_encode.encode None input' (Some output');
  execute_cesl resolve_exitval_f output' ecmaref show_exitval

let run (opts : options) : unit =
  let valid_langs = Enums.Lang.valid_langs langs opts.lang in
  match Enums.Lang.resolve_file_lang valid_langs opts.input with
  | Some JS ->
    execute_js Cmd_interpret.exitval_checker_esl opts.input opts.harness
      opts.ecmaref opts.show_exitval
  | Some CESL ->
    execute_cesl Cmd_interpret.exitval_checker_esl opts.input opts.ecmaref
      opts.show_exitval
  | _ ->
    execute_js Cmd_interpret.exitval_checker_esl opts.input opts.harness
      opts.ecmaref opts.show_exitval

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Config.Interpreter.verbose := opts.verbose;
  Config.Interpreter.verbose_at := opts.verbose_at;
  Config.Interpreter.debugger := opts.debugger;
  Cmd.eval_cmd (fun () -> run opts)
