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

let execute_cesl (input : Fpath.t) (ecmaref : Enums.ECMARef.t) : Val.t =
  let open Ecma_sl in
  let finterp = Enums.ECMARef.interp ecmaref in
  let interp = Parsing_utils.load_file finterp in
  let ast = Parsing_utils.load_file (Fpath.to_string input) in
  String.concat ";\n" [ ast; interp ]
  |> Parsing_utils.parse_prog
  |> Cmd_interpret.interpret Interpreter.Config.default

let execute_js (input : Fpath.t) (harness : Fpath.t option)
  (ecmaref : Enums.ECMARef.t) : Val.t =
  let input' = merge_input_harness input harness in
  let output' = Fpath.v (Filename.temp_file "ecmasl" "ast.js") in
  Cmd_encode.encode None input' (Some output');
  execute_cesl output' ecmaref

let run (opts : options) : unit =
  let valid_langs = Enums.Lang.valid_langs langs opts.lang in
  Cmd_interpret.process_exitval opts.show_exitval
  @@
  match Enums.Lang.resolve_file_lang valid_langs opts.input with
  | Some JS -> execute_js opts.input opts.harness opts.ecmaref
  | Some CESL -> execute_cesl opts.input opts.ecmaref
  | _ -> execute_js opts.input opts.harness opts.ecmaref

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Config.Interpreter.verbose := opts.verbose;
  Config.Interpreter.verbose_at := opts.verbose_at;
  Config.Interpreter.debugger := opts.debugger;
  Cmd.eval_cmd (fun () -> run opts)
