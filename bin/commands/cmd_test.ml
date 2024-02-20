open Ecma_sl

type options =
  { inputs : Fpath.t
  ; harness : Fpath.t option
  ; ecmaref : Enums.ECMARef.t
  }

module Test = struct
  let sucessful (input : Fpath.t) (retval : Val.t) : Val.t =
    Fmt.printf "Test Sucessful:           %a@." Fpath.pp input;
    retval

  let failure (input : Fpath.t) (retval : Val.t) : Val.t =
    Fmt.printf "Test Failure:             %a@." Fpath.pp input;
    retval

  let ecmaref_fail (input : Fpath.t) (retval : Val.t) : Val.t =
    Fmt.printf "Test Interpreter Failure: %a@." Fpath.pp input;
    retval

  let internal_fail (input : Fpath.t) : unit =
    Fmt.printf "Test Internal Failure:    %a@." Fpath.pp input
end

let jsreturn_checker (input : Fpath.t) (retval : Val.t) : Val.t =
  match retval with
  | Val.Tuple [ _; Val.Symbol "normal"; _; _ ] -> Test.sucessful input retval
  | _ -> Test.failure input retval

let exitval_checker_test (input : Fpath.t) (retval : Val.t) : Val.t =
  match retval with
  | Val.Tuple [ Val.Bool false; retval' ] -> jsreturn_checker input retval'
  | Val.Tuple [ Val.Bool true; _ ] -> Test.ecmaref_fail input retval
  | _ -> Test.ecmaref_fail input retval

let run_single (opts : options) (input : Fpath.t) (_ : Fpath.t option) : unit =
  ignore Enums.Lang.(resolve_file_lang [ JS ] input);
  let exitval_f = exitval_checker_test input in
  try Cmd_execute.execute_js exitval_f input opts.harness opts.ecmaref false
  with _ -> Test.internal_fail input

let run (opts : options) : unit = Dir.exec (run_single opts) opts.inputs None ""

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Config.Interpreter.verbose := false;
  Config.Interpreter.verbose_at := false;
  Config.Interpreter.debugger := false;
  Config.Interpreter.hide_prints := true;
  Cmd.eval_cmd (fun () -> run opts)
