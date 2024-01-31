open Ecma_sl

type options =
  { input : string
  ; interpret_lang : Lang.t
  ; interpret_verbose : bool
  ; interpret_verbose_at : bool
  ; interpret_debugger : bool
  ; interpret_main : string
  ; untyped : bool
  }

let langs : Lang.t list =
  [ Lang.Auto; Lang.ESL; Lang.CESL; Lang.CESLUnattached ]

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

let run_interpreter (check_result_f : Val.t -> unit) (main : string)
  (prog : Prog.t) : unit =
  let module Debugger = (val configure_debugger ()) in
  let module Verbose = (val configure_verbose ()) in
  let module Monitor = (val configure_monitor ()) in
  let module Interpreter = Interpreter.M (Debugger) (Verbose) (Monitor) in
  Interpreter.eval_prog ~main prog |> check_result_f

let no_exit_checker (_ : Val.t) : unit = ()

let esl_exit_checker (retval : Val.t) : unit =
  match retval with
  | Val.Tuple [ Val.Bool false; _ ] -> ()
  | Val.Tuple [ Val.Bool true; err ] ->
    Eslerr.(runtime (UncaughtExn (Val.str err)))
  | _ -> Eslerr.runtime (UnexpectedExitFmt retval)

let interpret_core (check_result_f : Val.t -> unit) (file : string)
  (main : string) : unit =
  Parsing_utils.load_file file
  |> Parsing_utils.parse_prog ~file
  |> run_interpreter check_result_f main

let compile_and_interpret (file : string) (main : string) : unit =
  Cmd_compile.run_compiler file |> run_interpreter esl_exit_checker main

let run (opts : options) : unit =
  let (input, main) = (opts.input, opts.interpret_main) in
  let valid_langs = Lang.valid langs opts.interpret_lang in
  match Cmd.test_file_ext valid_langs input with
  | Lang.ESL -> compile_and_interpret input main
  | Lang.CESL -> interpret_core esl_exit_checker input main
  | Lang.CESLUnattached -> interpret_core no_exit_checker input main
  | _ -> raise Cmd.(Command_error Failure)

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Config.Interpreter.verbose := opts.interpret_verbose;
  Config.Interpreter.verbose_at := opts.interpret_verbose_at;
  Config.Interpreter.debugger := opts.interpret_debugger;
  Config.Tesl.untyped := opts.untyped;
  Cmd.eval_cmd (fun () -> run opts)
