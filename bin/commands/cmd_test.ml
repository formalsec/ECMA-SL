open Ecma_sl

type options =
  { inputs : Fpath.t
  ; harness : Fpath.t option
  ; ecmaref : Enums.ECMARef.t
  }

let jsreturn_checker (input : Fpath.t) (retval : Val.t) : unit =
  match retval with
  | Val.Tuple [ _; Val.Symbol "normal"; _; _ ] ->
    Fmt.printf "Test Sucessful: %a@." Fpath.pp input
  | _ -> Fmt.printf "Test Failure: %a@." Fpath.pp input

let exitval_checker_test (input : Fpath.t) (retval : Val.t) : Val.t =
  match retval with
  | Val.Tuple [ Val.Bool false; retval' ] ->
    jsreturn_checker input retval';
    retval'
  | Val.Tuple [ Val.Bool true; err ] ->
    Eslerr.(runtime (UncaughtExn (Val.str err)))
  | _ -> Eslerr.runtime (UnexpectedExitFmt retval)

let run_single (opts : options) (input : Fpath.t) (_ : Fpath.t option) : unit =
  ignore Enums.Lang.(resolve_file_lang [ JS ] input);
  let resolve_exitval_f = exitval_checker_test input in
  Cmd_execute.execute_js resolve_exitval_f input opts.harness opts.ecmaref false

let run (opts : options) : unit = Dir.exec (run_single opts) opts.inputs None ""

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Config.Interpreter.verbose := false;
  Config.Interpreter.verbose_at := false;
  Config.Interpreter.debugger := false;
  Config.Interpreter.hide_prints := true;
  Cmd.eval_cmd (fun () -> run opts)
