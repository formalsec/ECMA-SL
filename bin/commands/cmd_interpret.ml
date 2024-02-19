open Ecma_sl

type options =
  { input : string
  ; interpret_lang : Enums.Lang.t
  ; interpret_verbose : bool
  ; interpret_verbose_at : bool
  ; interpret_debugger : bool
  ; interpret_main : string
  ; interpret_show_result : bool
  ; untyped : bool
  }

let langs : Enums.Lang.t list = Enums.Lang.[ Auto; ESL; CESL; CESLUnattached ]

let configure_debugger () : (module Debugger.M) =
  match !Config.Interpreter.debugger with
  | true -> (module Debugger.Enable : Debugger.M)
  | false -> (module Debugger.Disable : Debugger.M)

let configure_verbose () : (module Verbose.M) =
  match !Config.Interpreter.verbose with
  | true -> (module Verbose.Enable : Verbose.M)
  | false -> (module Verbose.Disable : Verbose.M)

let configure_monitor () : (module Monitor.M) =
  (module Monitor.Default : Monitor.M)

let run_interpreter (main : string) (prog : Prog.t) : Val.t =
  let module Debugger = (val configure_debugger ()) in
  let module Verbose = (val configure_verbose ()) in
  let module Monitor = (val configure_monitor ()) in
  let module Interpreter = Interpreter.M (Debugger) (Verbose) (Monitor) in
  Interpreter.eval_prog ~main prog

let no_exit_checker (retval : Val.t) : Val.t = retval

let esl_exit_checker (retval : Val.t) : Val.t =
  match retval with
  | Val.Tuple [ Val.Bool false; retval' ] -> retval'
  | Val.Tuple [ Val.Bool true; err ] ->
    Eslerr.(runtime (UncaughtExn (Val.str err)))
  | _ -> Eslerr.runtime (UnexpectedExitFmt retval)

let process_result (check_result_f : Val.t -> Val.t) (show_result : bool)
  (retval : Val.t) : unit =
  let retval' = check_result_f retval in
  if show_result then Cmd.log "retval = %a" Ecma_sl.Val.pp retval'

let interpret_core (check_result_f : Val.t -> Val.t) (opts : options) : unit =
  Parsing_utils.load_file opts.input
  |> Parsing_utils.parse_prog ~file:opts.input
  |> run_interpreter opts.interpret_main
  |> process_result check_result_f opts.interpret_show_result

let compile_and_interpret (opts : options) : unit =
  Cmd_compile.run_compiler opts.input
  |> run_interpreter opts.interpret_main
  |> process_result esl_exit_checker opts.interpret_show_result

let run (opts : options) : unit =
  let open Enums.Lang in
  let valid_langs = valid langs opts.interpret_lang in
  match Cmd.test_file_ext valid_langs opts.input with
  | ESL -> compile_and_interpret opts
  | CESL -> interpret_core esl_exit_checker opts
  | CESLUnattached -> interpret_core no_exit_checker opts
  | _ -> raise Cmd.(Command_error Failure)

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Config.Interpreter.verbose := opts.interpret_verbose;
  Config.Interpreter.verbose_at := opts.interpret_verbose_at;
  Config.Interpreter.debugger := opts.interpret_debugger;
  Config.Tesl.untyped := opts.untyped;
  Cmd.eval_cmd (fun () -> run opts)
