open Ecma_sl

module Options = struct
  let langs : Lang.t list = Lang.[ Auto; ESL; CESL; CESLUnattached ]
  let debugger = ref false
  let verbose = ref false

  type t =
    { input : Fpath.t
    ; lang : Lang.t
    ; main : string
    ; show_exitval : bool
    }

  let set_options input lang verbose' debugger' main show_exitval untyped' =
    Cmd_compile.Options.untyped := untyped';
    debugger := debugger';
    verbose := verbose';
    { input; lang; main; show_exitval }
end

module InterpreterConfig = struct
  let debugger () : (module Debugger.M) =
    match !Options.debugger with
    | true -> (module Debugger.Enable : Debugger.M)
    | false -> (module Debugger.Disable : Debugger.M)

  let verbose () : (module Verbose.M) =
    match !Options.verbose with
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
  let retval = fst (interpret_partial config prog) in
  Log.debug "Sucessfuly evaluated program with return '%a'." Val.pp retval;
  retval

let interpret_cesl (config : Interpreter.Config.t) (input : Fpath.t) : Val.t =
  let input' = Fpath.to_string input in
  Parsing.load_file input'
  |> Parsing.parse_prog ~file:input'
  |> interpret config

let interpret_esl (config : Interpreter.Config.t) (input : Fpath.t) : Val.t =
  Cmd_compile.compile input |> interpret config

let process_exitval (show_exitval : bool) (exitval : Val.t) : unit =
  if show_exitval then Log.app "» exit value: %a\n" Val.pp exitval

let run (opts : Options.t) : unit =
  let valid_langs = Lang.valid_langs Options.langs opts.lang in
  let config = { Interpreter.Config.default with main = opts.main } in
  process_exitval opts.show_exitval
  @@
  match Lang.resolve_file_lang valid_langs opts.input with
  | Some ESL -> interpret_esl config opts.input
  | Some CESL -> interpret_cesl config opts.input
  | Some CESLUnattached | _ ->
    interpret_cesl { config with resolve_exitval = false } opts.input

let main () (opts : Options.t) : int = Cmd.eval_cmd (fun () -> run opts)
