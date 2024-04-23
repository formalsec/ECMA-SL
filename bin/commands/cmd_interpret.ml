open Ecma_sl

module Options = struct
  let langs : Enums.Lang.t list = Enums.Lang.[ Auto; ESL; CESL; CESLUnattached ]

  type tracer =
    { mode : Enums.InterpTracer.t
    ; loc : bool
    ; depth : int
    }

  type instrument =
    { tracer : tracer
    ; debugger : bool
    }

  let default_instrument () : instrument =
    { tracer = { mode = None; loc = false; depth = 0 }; debugger = false }

  type t =
    { input : Fpath.t
    ; lang : Enums.Lang.t
    ; instrument : instrument
    ; main : string
    ; show_exitval : bool
    ; untyped : bool
    }

  let set (input : Fpath.t) (lang : Enums.Lang.t)
    (tracer_mode : Enums.InterpTracer.t) (tracer_loc : bool)
    (tracer_depth : int) (debugger : bool) (main : string) (show_exitval : bool)
    (untyped : bool) : t =
    let (mode, loc, depth) = (tracer_mode, tracer_loc, tracer_depth) in
    let instrument = { tracer = { mode; loc; depth }; debugger } in
    { input; lang; instrument; main; show_exitval; untyped }
end

module InterpreterInstrument = struct
  let tracer ({ mode; loc; depth } : Options.tracer) : (module Tracer.M) =
    Tracer.Config.trace_loc := loc;
    Tracer.Config.trace_depth := if depth > 0 then Some depth else None;
    match mode with
    | None -> (module Tracer.Disable : Tracer.M)
    | Call -> (module Tracer.Call : Tracer.M)
    | Step -> (module Tracer.Step : Tracer.M)
    | Full -> (module Tracer.Full : Tracer.M)
    | Core -> (module Tracer.Core : Tracer.M)

  let debugger (debugger : bool) : (module Debugger.M) =
    match debugger with
    | true -> (module Debugger.Enable : Debugger.M)
    | false -> (module Debugger.Disable : Debugger.M)

  let monitor () : (module Monitor.M) = (module Monitor.Default : Monitor.M)

  let intrument (instrument : Options.instrument) : (module Instrument.M) =
    let module Tracer = (val tracer instrument.tracer) in
    let module Debugger = (val debugger instrument.debugger) in
    let module Monitor = (val monitor ()) in
    (module Instrument.Default (Tracer) (Debugger) (Monitor))
end

let interpret_partial (entry : Interpreter.EntryPoint.t)
  (instrument : Options.instrument) (prog : Prog.t) : Val.t * Val.t Heap.t =
  let module Instrument = (val InterpreterInstrument.intrument instrument) in
  let module Interpreter = Interpreter.M (Instrument) in
  Interpreter.eval_partial entry prog

let interpret (entry : Interpreter.EntryPoint.t)
  (instrument : Options.instrument) (prog : Prog.t) : Val.t =
  let retval = fst (interpret_partial entry instrument prog) in
  Log.debug "Sucessfuly evaluated program with return '%a'." Val.pp retval;
  retval

let interpret_cesl (entry : Interpreter.EntryPoint.t)
  (instrument : Options.instrument) (file : Fpath.t) : Val.t =
  let file' = Fpath.to_string file in
  Parsing.load_file file'
  |> Parsing.parse_prog ~file:file'
  |> interpret entry instrument

let interpret_esl (entry : Interpreter.EntryPoint.t)
  (instrument : Options.instrument) (untyped : bool) (file : Fpath.t) : Val.t =
  Cmd_compile.compile untyped file |> interpret entry instrument

let process_exitval (show_exitval : bool) (exitval : Val.t) : unit =
  if show_exitval then Log.out "» exit value: %a@." Val.pp exitval

let run () (opts : Options.t) : unit =
  let valid_langs = Enums.Lang.valid_langs Options.langs opts.lang in
  let entry = { Interpreter.EntryPoint.default with main = opts.main } in
  process_exitval opts.show_exitval
  @@
  match Enums.Lang.resolve_file_lang valid_langs opts.input with
  | Some ESL -> interpret_esl entry opts.instrument opts.untyped opts.input
  | Some CESL -> interpret_cesl entry opts.instrument opts.input
  | Some CESLUnattached | _ ->
    let entry' = { entry with resolve_exitval = false } in
    interpret_cesl entry' opts.instrument opts.input
