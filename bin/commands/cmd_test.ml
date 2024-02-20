open Ecma_sl

type options =
  { inputs : string
  ; harness : string option
  ; execute_lang : Enums.Lang.t
  ; execute_version : Enums.ECMARef.t
  }

let langs : Enums.Lang.t list = Enums.Lang.[ Auto; JS; CESL ]

let make_execute_opts (opts : options) : Cmd_execute.options =
  { input = opts.inputs
  ; harness = opts.harness
  ; execute_lang = opts.execute_lang
  ; execute_version = opts.execute_version
  ; interpret_verbose = false
  ; interpret_verbose_at = false
  ; interpret_debugger = false
  ; interpret_show_result = false
  ; interpret_hide_prints = true
  }

let check_js_return (input : string) (retval : Val.t) : unit =
  match retval with
  | Val.Tuple [ _; Val.Symbol "normal"; _; _ ] ->
    Fmt.printf "Test Sucessful: %s@." input
  | _ -> Fmt.printf "Test Failure: %s@." input

let test_checker (input : string) (retval : Val.t) : Val.t =
  match retval with
  | Val.Tuple [ Val.Bool false; retval' ] ->
    check_js_return input retval';
    retval'
  | Val.Tuple [ Val.Bool true; err ] ->
    Eslerr.(runtime (UncaughtExn (Val.str err)))
  | _ -> Eslerr.runtime (UnexpectedExitFmt retval)

let run (opts : options) : unit =
  let open Enums.Lang in
  let valid_langs = valid langs opts.execute_lang in
  let encode_opts = make_execute_opts opts in
  let run_single input _ =
    let opts = { encode_opts with input } in
    let exit_checker_f = test_checker input in
    match Cmd.test_file_ext valid_langs input with
    | Some JS -> Cmd_execute.encode_and_execute ~exit_checker_f opts
    | Some CESL -> Cmd_execute.execute ~exit_checker_f opts
    | _ -> Cmd_execute.execute ~exit_checker_f opts
  in
  Dir.exec run_single opts.inputs None ""

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Config.Interpreter.hide_prints := true;
  Cmd.eval_cmd (fun () -> run opts)
