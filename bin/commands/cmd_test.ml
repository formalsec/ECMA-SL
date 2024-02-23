open Ecma_sl

type options =
  { inputs : Fpath.t
  ; harness : Fpath.t option
  ; ecmaref : Enums.ECMARef.t
  }

module Test = struct
  type out_channels =
    { out : Unix.file_descr
    ; err : Unix.file_descr
    ; devnull : Unix.file_descr
    }

  let hide_out_channels () : out_channels =
    let out = Unix.dup Unix.stdout in
    let err = Unix.dup Unix.stderr in
    let devnull = Unix.openfile "/dev/null" [ O_WRONLY ] 0o666 in
    let old_channels = { out; err; devnull } in
    Unix.dup2 devnull Unix.stdout;
    Unix.dup2 devnull Unix.stderr;
    old_channels

  let restore_out_channels (channels : out_channels) : unit =
    Unix.dup2 channels.out Unix.stdout;
    Unix.dup2 channels.err Unix.stderr;
    Unix.close channels.out;
    Unix.close channels.err;
    Unix.close channels.devnull

  let header () : unit =
    Fmt.printf "%a@."
      (Font.pp_text_out [ Font.Cyan ])
      "----------------------------------------\n\
      \              ECMA-SL Test\n\
       ----------------------------------------"

  let log (out_channels : out_channels) (input : Fpath.t) (font : Font.t list)
    (header : string) : unit =
    restore_out_channels out_channels;
    Fmt.printf "%a %a@." (Font.pp_text_out font) header
      (Font.pp_out [ Faint ] Fpath.pp)
      input

  let sucessful (out_channels : out_channels) (input : Fpath.t) : unit =
    log out_channels input [ Font.Green ] "Test Successful:"

  let failure (out_channels : out_channels) (input : Fpath.t) : unit =
    log out_channels input [ Red ] "Test Failure:"

  let ecmaref_fail (out_channels : out_channels) (input : Fpath.t) : unit =
    log out_channels input [ Purple ] "Interpreter Failure:"

  let internal_fail (out_channels : out_channels) (input : Fpath.t) : unit =
    log out_channels input [ Purple ] "Internal Failure:"
end

let test_input (out_channels : Test.out_channels)
  (setup : Prog.t * Val.t Heap.t option) (input : Fpath.t) : unit =
  let retval = Cmd_execute.execute_js setup input in
  match retval with
  | Val.Tuple [ _; Val.Symbol "normal"; _; _ ] ->
    Test.sucessful out_channels input
  | _ -> Test.failure out_channels input

let run_single (setup : Prog.t * Val.t Heap.t option) (input : Fpath.t)
  (_ : Fpath.t option) : unit =
  ignore Enums.Lang.(resolve_file_lang [ JS ] input);
  let out_channels = Test.hide_out_channels () in
  try test_input out_channels setup input with
  | Eslerr.Runtime_error { msgs = UncaughtExn _ :: []; _ } ->
    Test.ecmaref_fail out_channels input
  | _ -> Test.internal_fail out_channels input

let run (opts : options) : unit =
  Test.header ();
  let setup = Cmd_execute.setup_execution opts.ecmaref opts.harness in
  Dir.exec (run_single setup) opts.inputs None ""

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Config.Interpreter.verbose := false;
  Config.Interpreter.verbose_at := false;
  Config.Interpreter.debugger := false;
  Cmd.eval_cmd (fun () -> run opts)
