open Ecma_sl

module Options = struct
  type t =
    { inputs : Fpath.t
    ; lang : Enums.Lang.t
    ; jsinterp : Enums.JSInterp.t
    ; harness : Fpath.t option
    }

  let set (inputs : Fpath.t) (lang : Enums.Lang.t) (jsinterp : Enums.JSInterp.t)
    (harness : Fpath.t option) : t =
    { inputs; lang; jsinterp; harness }
end

module Test = struct
  let header () : unit =
    Log.out "%a@."
      (Font.pp_text_out [ Font.Cyan ])
      "----------------------------------------\n\
      \              ECMA-SL Test\n\
       ----------------------------------------"

  let log (streams : Log.Redirect.t) (input : Fpath.t) (font : Font.t)
    (header : string) : unit =
    Log.Redirect.restore streams;
    Log.out "%a %a@." (Font.pp_text_out font) header
      (Font.pp_out [ Faint ] Fpath.pp)
      input

  let sucessful (streams : Log.Redirect.t) (input : Fpath.t) : unit =
    log streams input [ Green ] "Test Successful:"

  let failure (streams : Log.Redirect.t) (input : Fpath.t) : unit =
    log streams input [ Red ] "Test Failure:"

  let interp_fail (streams : Log.Redirect.t) (input : Fpath.t) : unit =
    log streams input [ Purple ] "Interpreter Failure:"

  let internal_fail (streams : Log.Redirect.t) (input : Fpath.t) : unit =
    log streams input [ Purple ] "Internal Failure:"
end

let test_input (streams : Log.Redirect.t) (setup : Prog.t * Val.t Heap.t option)
  (input : Fpath.t) : unit =
  let instrument = Cmd_interpret.Options.default_instrument () in
  match Cmd_execute.execute_js setup instrument input with
  | Val.Tuple [ _; Val.Symbol "normal"; _; _ ] -> Test.sucessful streams input
  | _ -> Test.failure streams input

let run_single (setup : Prog.t * Val.t Heap.t option) (input : Fpath.t)
  (_ : Fpath.t option) : unit =
  ignore Enums.Lang.(resolve_file_lang [ JS ] input);
  let streams = Log.Redirect.capture Null in
  try test_input streams setup input with
  | Runtime_error.Error { msgs = UncaughtExn _ :: []; _ } ->
    Test.interp_fail streams input
  | _ -> Test.internal_fail streams input

let setup_tests (jsinterp : Enums.JSInterp.t) (harness : Fpath.t option) :
  Prog.t * Val.t Heap.t option =
  let instrument = Cmd_interpret.Options.default_instrument () in
  Cmd_execute.setup_execution jsinterp harness instrument

let run () (opts : Options.t) : unit =
  Test.header ();
  let setup = setup_tests opts.jsinterp opts.harness in
  Files.exec (run_single setup) opts.inputs None ""
