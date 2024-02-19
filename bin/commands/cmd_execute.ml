open Ecma_sl

type options =
  { input : string
  ; execute_lang : Enums.Lang.t
  ; execute_version : Enums.ECMARef.t
  ; interpret_verbose : bool
  ; interpret_verbose_at : bool
  ; interpret_debugger : bool
  ; interpret_show_result : bool
  }

let langs : Enums.Lang.t list = Enums.Lang.[ Auto; JS; CESL ]

let execute (opts : options) : unit =
  let open Ecma_sl in
  let finterp = Enums.ECMARef.interp opts.execute_version in
  let interp = Parsing_utils.load_file finterp in
  let ast = Parsing_utils.load_file opts.input in
  String.concat ";\n" [ ast; interp ]
  |> Parsing_utils.parse_prog
  |> Cmd_interpret.run_interpreter "main"
  |> Cmd_interpret.process_result Cmd_interpret.esl_exit_checker
       opts.interpret_show_result

let encode_and_execute (opts : options) : unit =
  let output = "/tmp/ecmasl-ast.cesl" in
  Cmd_encode.encode opts.input (Some output) None;
  execute { opts with input = output }

let run (opts : options) : unit =
  let open Enums.Lang in
  let valid_langs = valid langs opts.execute_lang in
  match Cmd.test_file_ext valid_langs opts.input with
  | Some JS -> encode_and_execute opts
  | Some CESL -> execute opts
  | _ -> encode_and_execute opts

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Config.Interpreter.verbose := opts.interpret_verbose;
  Config.Interpreter.verbose_at := opts.interpret_verbose_at;
  Config.Interpreter.debugger := opts.interpret_debugger;
  Cmd.eval_cmd (fun () -> run opts)
