open Ecma_sl

type options =
  { input : Fpath.t
  ; lang : Enums.Lang.t
  ; verbose : bool
  ; verbose_at : bool
  ; debugger : bool
  ; main : string
  ; show_exitval : bool
  ; hide_prints : bool
  ; untyped : bool
  }

let langs : Enums.Lang.t list = Enums.Lang.[ Auto; ESL; CESL; CESLUnattached ]

module InterpreterConfig = struct
  let debugger () : (module Debugger.M) =
    match !Config.Interpreter.debugger with
    | true -> (module Debugger.Enable : Debugger.M)
    | false -> (module Debugger.Disable : Debugger.M)

  let verbose () : (module Verbose.M) =
    match !Config.Interpreter.verbose with
    | true -> (module Verbose.Enable : Verbose.M)
    | false -> (module Verbose.Disable : Verbose.M)

  let monitor () : (module Monitor.M) = (module Monitor.Default : Monitor.M)
end

let exitval_checker_none (retval : Val.t) : Val.t = retval

let exitval_checker_esl (retval : Val.t) : Val.t =
  match retval with
  | Val.Tuple [ Val.Bool false; retval' ] -> retval'
  | Val.Tuple [ Val.Bool true; err ] ->
    Eslerr.(runtime (UncaughtExn (Val.str err)))
  | _ -> Eslerr.runtime (UnexpectedExitFmt retval)

let interpret (main : string) (prog : Prog.t) : Val.t =
  let module Debugger = (val InterpreterConfig.debugger ()) in
  let module Verbose = (val InterpreterConfig.verbose ()) in
  let module Monitor = (val InterpreterConfig.monitor ()) in
  let module Interpreter = Interpreter.M (Debugger) (Verbose) (Monitor) in
  Interpreter.eval_prog ~main prog

let process_exitval (resolve_exitval_f : Val.t -> Val.t) (show_exitval : bool)
  (exitval : Val.t) : unit =
  let exitval' = resolve_exitval_f exitval in
  if show_exitval then Log.app "» exit value: %a\n" Ecma_sl.Val.pp exitval'

let interpret_cesl (resolve_exitval_f : Val.t -> Val.t) (input : Fpath.t)
  (main : string) (show_exitval : bool) : unit =
  let input' = Fpath.to_string input in
  Parsing_utils.load_file input'
  |> Parsing_utils.parse_prog ~file:input'
  |> interpret main
  |> process_exitval resolve_exitval_f show_exitval

let interpret_esl (input : Fpath.t) (main : string) (show_exitval : bool) : unit
    =
  Cmd_compile.compile input
  |> interpret main
  |> process_exitval exitval_checker_esl show_exitval

let run (opts : options) : unit =
  let valid_langs = Enums.Lang.valid_langs langs opts.lang in
  match Enums.Lang.resolve_file_lang valid_langs opts.input with
  | Some ESL -> interpret_esl opts.input opts.main opts.show_exitval
  | Some CESL ->
    interpret_cesl exitval_checker_esl opts.input opts.main opts.show_exitval
  | Some CESLUnattached ->
    interpret_cesl exitval_checker_none opts.input opts.main opts.show_exitval
  | _ -> interpret_esl opts.input opts.main opts.show_exitval

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Config.Interpreter.verbose := opts.verbose;
  Config.Interpreter.verbose_at := opts.verbose_at;
  Config.Interpreter.debugger := opts.debugger;
  Config.Interpreter.hide_prints := opts.hide_prints;
  Config.Tesl.untyped := opts.untyped;
  Cmd.eval_cmd (fun () -> run opts)
