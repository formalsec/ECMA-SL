open Ecma_sl

module Options = struct
  type t =
    { inputs : Fpath.t
    ; harness : Fpath.t option
    ; lang : Enums.Lang.t
    ; ecmaref : Enums.JSInterp.t
    }

  let set inputs lang ecmaref harness =
    Cmd_compile.Options.untyped := true;
    Cmd_interpret.Options.trace := None;
    Cmd_interpret.Options.trace_loc := false;
    Cmd_interpret.Options.trace_depth := 0;
    Cmd_interpret.Options.debugger := false;
    { inputs; lang; ecmaref; harness }
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

  let ecmaref_fail (streams : Log.Redirect.t) (input : Fpath.t) : unit =
    log streams input [ Purple ] "Interpreter Failure:"

  let internal_fail (streams : Log.Redirect.t) (input : Fpath.t) : unit =
    log streams input [ Purple ] "Internal Failure:"
end

let test_input (streams : Log.Redirect.t) (setup : Prog.t * Val.t Heap.t option)
  (input : Fpath.t) : unit =
  match Cmd_execute.execute_js setup input with
  | Val.Tuple [ _; Val.Symbol "normal"; _; _ ] -> Test.sucessful streams input
  | _ -> Test.failure streams input

let run_single (setup : Prog.t * Val.t Heap.t option) (input : Fpath.t)
  (_ : Fpath.t option) : unit =
  ignore Enums.Lang.(resolve_file_lang [ JS ] input);
  let streams = Log.Redirect.capture Null in
  try test_input streams setup input with
  | Runtime_error.Error { msgs = UncaughtExn _ :: []; _ } ->
    Test.ecmaref_fail streams input
  | _ -> Test.internal_fail streams input

let run (opts : Options.t) : unit =
  Test.header ();
  let setup = Cmd_execute.setup_execution opts.ecmaref opts.harness in
  Files.exec (run_single setup) opts.inputs None ""

let main () (opts : Options.t) : int = Cmd.eval_cmd (fun () -> run opts)
