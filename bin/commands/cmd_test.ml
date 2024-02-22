open Ecma_sl

type options =
  { inputs : Fpath.t
  ; harness : Fpath.t option
  ; ecmaref : Enums.ECMARef.t
  }

module Output = struct
  open Unix

  type t =
    { out : file_descr
    ; err : file_descr
    ; devnull : file_descr
    }

  let hide () : t =
    let out = dup stdout in
    let err = dup stderr in
    let devnull = openfile "/dev/null" [ O_WRONLY ] 0o666 in
    let old_channels = { out; err; devnull } in
    dup2 devnull stdout;
    dup2 devnull stderr;
    old_channels

  let restore (old_channels : t) : unit =
    dup2 old_channels.out Unix.stdout;
    dup2 old_channels.err Unix.stderr;
    close old_channels.out;
    close old_channels.err;
    close old_channels.devnull
end

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

let exitval_checker_test (out_channels : Output.t) (input : Fpath.t)
  (retval : Val.t) : Val.t =
  Output.restore out_channels;
  match retval with
  | Val.Tuple [ Val.Bool false; retval' ] -> jsreturn_checker input retval'
  | Val.Tuple [ Val.Bool true; _ ] -> Test.ecmaref_fail input retval
  | _ -> Test.ecmaref_fail input retval

let run_test (opts : options) (input : Fpath.t) : unit =
  let out_channels = Output.hide () in
  let exitval_f = exitval_checker_test out_channels input in
  Cmd_execute.execute_js exitval_f input opts.harness opts.ecmaref false

let run_single (opts : options) (input : Fpath.t) (_ : Fpath.t option) : unit =
  ignore Enums.Lang.(resolve_file_lang [ JS ] input);
  try run_test opts input with _ -> Test.internal_fail input

let run (opts : options) : unit = Dir.exec (run_single opts) opts.inputs None ""

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Config.Interpreter.verbose := false;
  Config.Interpreter.verbose_at := false;
  Config.Interpreter.debugger := false;
  Cmd.eval_cmd (fun () -> run opts)
