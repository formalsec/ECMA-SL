open Ecma_sl
open Syntax.Result

module Options = struct
  type t =
    { inputs : Fpath.t list
    ; lang : Enums.Lang.t
    ; jsinterp : Enums.JSInterp.t
    ; harness : Fpath.t option
    }

  let set (inputs : Fpath.t list) (lang : Enums.Lang.t)
    (jsinterp : Enums.JSInterp.t) (harness : Fpath.t option) : t =
    { inputs; lang; jsinterp; harness }
end

module Test = struct
  let header () : unit =
    Log.out "%a@."
      (Font.pp_text_out [ Font.Cyan ])
      "----------------------------------------\n\
      \              ECMA-SL Test\n\
       ----------------------------------------"

  let log (streams : Log.Redirect.t) (ftest : Fpath.t) (font : Font.t)
    (header : string) : unit =
    Log.Redirect.restore streams;
    Log.out "%a %a@." (Font.pp_text_out font) header
      (Font.pp_out [ Faint ] Fpath.pp)
      ftest

  let sucessful (streams : Log.Redirect.t) (ftest : Fpath.t) : unit Result.t =
    Ok (log streams ftest [ Green ] "Test Successful:")

  let failure (streams : Log.Redirect.t) (ftest : Fpath.t) : unit Result.t =
    log streams ftest [ Red ] "Test Failure:";
    Error `Test

  let runtime_failure (streams : Log.Redirect.t) (ftest : Fpath.t) :
    unit Result.t =
    log streams ftest [ Purple ] "Interpreter Failure:";
    Error `Test

  let internal_failure (streams : Log.Redirect.t) (ftest : Fpath.t) :
    unit Result.t =
    log streams ftest [ Purple ] "Internal Failure:";
    Error `Test

  let unexpected_evaluation (streams : Log.Redirect.t) (ftest : Fpath.t) :
    unit Result.t =
    log streams ftest [ Purple ] "Unexpected Evaluation:";
    Error `Test
end

let setup_tests (jsinterp : Enums.JSInterp.t) (harness : Fpath.t option) :
  (Prog.t * Val.t Heap.t option) Result.t =
  let instrument = Cmd_interpret.Options.default_instrument () in
  Cmd_execute.setup_execution jsinterp harness instrument

let test_input (streams : Log.Redirect.t) (setup : Prog.t * Val.t Heap.t option)
  (ftest : Fpath.t) : unit Result.t =
  let instrument = Cmd_interpret.Options.default_instrument () in
  match Cmd_execute.execute_js setup instrument ftest with
  | Ok (Tuple [ _; Symbol "normal"; _; _ ]) -> Test.sucessful streams ftest
  | Ok _ -> Test.failure streams ftest
  | Error (`Internal _) -> Test.internal_failure streams ftest
  | Error (`Runtime _) -> Test.runtime_failure streams ftest
  | Error _ -> Test.unexpected_evaluation streams ftest

let run_single (setup : Prog.t * Val.t Heap.t option) (ftest : Fpath.t)
  (_ : Fpath.t option) : unit Result.t =
  let streams = Log.Redirect.capture Null in
  ignore Enums.Lang.(resolve_file_lang [ JS ] ftest);
  test_input streams setup ftest

let run () (opts : Options.t) : unit Result.t =
  Test.header ();
  let* setup = setup_tests opts.jsinterp opts.harness in
  Files.exec_multiple (run_single setup) opts.inputs None ""
