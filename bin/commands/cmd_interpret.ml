open Ecma_sl

type options =
  { input : Fpath.t
  ; lang : Enums.Lang.t
  ; verbose : bool
  ; verbose_at : bool
  ; debugger : bool
  ; main : string
  ; show_exitval : bool
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

let interpret_partial (config : Interpreter.Config.t) (prog : Prog.t) :
  Val.t * Val.t Heap.t =
  let module Debugger = (val InterpreterConfig.debugger ()) in
  let module Verbose = (val InterpreterConfig.verbose ()) in
  let module Monitor = (val InterpreterConfig.monitor ()) in
  let module Interpreter = Interpreter.M (Debugger) (Verbose) (Monitor) in
  Interpreter.eval_partial config prog

let interpret (config : Interpreter.Config.t) (prog : Prog.t) : Val.t =
  fst (interpret_partial config prog)

let interpret_cesl (config : Interpreter.Config.t) (input : Fpath.t) : Val.t =
  let input' = Fpath.to_string input in
  Parsing_utils.load_file input'
  |> Parsing_utils.parse_prog ~file:input'
  |> interpret config

let interpret_esl (config : Interpreter.Config.t) (input : Fpath.t) : Val.t =
  Cmd_compile.compile input |> interpret config

let process_exitval (show_exitval : bool) (exitval : Val.t) : unit =
  if show_exitval then Log.app "» exit value: %a\n" Ecma_sl.Val.pp exitval

let run (opts : options) : unit =
  let valid_langs = Enums.Lang.valid_langs langs opts.lang in
  let interp_config = { Interpreter.Config.default with main = opts.main } in
  process_exitval opts.show_exitval
  @@
  match Enums.Lang.resolve_file_lang valid_langs opts.input with
  | Some ESL -> interpret_esl interp_config opts.input
  | Some CESL -> interpret_cesl interp_config opts.input
  | Some CESLUnattached ->
    interpret_cesl { interp_config with resolve_exitval = false } opts.input
  | _ -> interpret_esl interp_config opts.input

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Config.Interpreter.verbose := opts.verbose;
  Config.Interpreter.verbose_at := opts.verbose_at;
  Config.Interpreter.debugger := opts.debugger;
  Config.Tesl.untyped := opts.untyped;
  Cmd.eval_cmd (fun () -> run opts)
