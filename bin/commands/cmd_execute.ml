open Ecma_sl

type options =
  { input : string
  ; harness : string option
  ; execute_lang : Enums.Lang.t
  ; execute_version : Enums.ECMARef.t
  ; interpret_verbose : bool
  ; interpret_verbose_at : bool
  ; interpret_debugger : bool
  ; interpret_show_result : bool
  ; interpret_hide_prints : bool
  }

let langs : Enums.Lang.t list = Enums.Lang.[ Auto; JS; CESL ]

let merge_input_harness (opts : options) : string =
  match opts.harness with
  | None -> opts.input
  | Some harness ->
    let input_code = Io.read_file opts.input in
    let harness_code = Io.read_file harness in
    let file = "/tmp/ecmasl-ast.js" in
    Io.write_file file (harness_code ^ input_code);
    file

let execute ?(exit_checker_f : Val.t -> Val.t = Cmd_interpret.esl_exit_checker)
  (opts : options) : unit =
  let open Ecma_sl in
  let finterp = Enums.ECMARef.interp opts.execute_version in
  let interp = Parsing_utils.load_file finterp in
  let ast = Parsing_utils.load_file opts.input in
  String.concat ";\n" [ ast; interp ]
  |> Parsing_utils.parse_prog
  |> Cmd_interpret.run_interpreter "main"
  |> Cmd_interpret.process_result exit_checker_f opts.interpret_show_result

let encode_and_execute
  ?(exit_checker_f : Val.t -> Val.t = Cmd_interpret.esl_exit_checker)
  (opts : options) : unit =
  let input = merge_input_harness opts in
  let output = "/tmp/ecmasl-ast.cesl" in
  Cmd_encode.encode None input (Some output);
  execute ~exit_checker_f { opts with input = output }

let run (opts : options) : unit =
  let open Enums.Lang in
  let valid_langs = valid langs opts.execute_lang in
  match Cmd.test_file_ext valid_langs opts.input with
  | Some CESL -> execute opts
  | Some JS | _ -> encode_and_execute opts

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Config.Interpreter.verbose := opts.interpret_verbose;
  Config.Interpreter.verbose_at := opts.interpret_verbose_at;
  Config.Interpreter.debugger := opts.interpret_debugger;
  Config.Interpreter.hide_prints := opts.interpret_hide_prints;
  Cmd.eval_cmd (fun () -> run opts)
