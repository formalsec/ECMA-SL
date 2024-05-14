open Ecma_sl
open Syntax.Result

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
    ; profiler : Enums.InterpProfiler.t
    }

  let set_instrument (tracer_mode : Enums.InterpTracer.t) (tracer_loc : bool)
    (tracer_depth : int) (debugger : bool) (profiler : Enums.InterpProfiler.t) :
    instrument =
    let (mode, loc, depth) = (tracer_mode, tracer_loc, tracer_depth) in
    { tracer = { mode; loc; depth }; debugger; profiler }

  type config =
    { print_depth : int option
    ; resolve_exitval : bool
    ; show_exitval : bool
    ; instrument : instrument
    }

  let default_config () : config =
    let tracer = { mode = None; loc = false; depth = 0 } in
    { print_depth = None
    ; resolve_exitval = true
    ; show_exitval = false
    ; instrument = { tracer; debugger = false; profiler = None }
    }

  let set_config (print_depth : int option) (show_exitval : bool)
    (instrument : instrument) : config =
    let resolve_exitval = true in
    { print_depth; resolve_exitval; show_exitval; instrument }

  type t =
    { input : Fpath.t
    ; lang : Enums.Lang.t
    ; main : string
    ; untyped : bool
    ; config : config
    }

  let set (input : Fpath.t) (lang : Enums.Lang.t) (main : string)
    (untyped : bool) (config : config) : t =
    { input; lang; main; untyped; config }
end

module InterpreterMetrics = struct
  open Yojson.Basic
  open Yojson.Basic.Util

  let pp_el (pp : Fmt.t -> t -> unit) (fmt : Fmt.t) (member : t) : unit =
    if member != `Null then pp fmt member

  let pp_timer (fmt : Fmt.t) (timer : t) : unit =
    let exec_time = member "exec_time" timer |> to_float in
    let (_, _, secs, millis) = Base.format_time exec_time in
    Fmt.fprintf fmt "@\nexec time:  %ds%.3dms" secs millis

  let pp_memory (fmt : Fmt.t) (memory : t) : unit =
    let heap_n = member "heap_objs" memory |> to_int in
    let heap_sz = member "heap_size" memory |> to_int in
    let heap_sz_bytes = heap_sz * Sys.word_size in
    let (heap_sz_fmt, heap_sz_unit) = Base.format_bytes heap_sz_bytes in
    Fmt.fprintf fmt "@\nobj allocs: %d@\nheap size:  %d bytes (~%0.2f %s)"
      heap_n heap_sz_bytes heap_sz_fmt heap_sz_unit

  let pp_counter (fmt : Fmt.t) (counter : t) : unit =
    let divider = "----------" in
    let calls = member "func_calls" counter |> to_int in
    let stmts = member "stmt_evals" counter |> to_int in
    let exprs = member "expr_evals" counter |> to_int in
    Fmt.fprintf fmt "@\n%s@\nfunc calls: %d@\nstmt evals: %d@\nexpr evals: %d"
      divider calls stmts exprs

  let pp (fmt : Fmt.t) (metrics : t) : unit =
    let timer = Util.member "timer" metrics in
    let memory = Util.member "memory" metrics in
    let counter = Util.member "counter" metrics in
    Fmt.fprintf fmt "%a%a%a" (pp_el pp_timer) timer (pp_el pp_memory) memory
      (pp_el pp_counter) counter

  let log (profiler : Enums.InterpProfiler.t) (metrics : t) : unit =
    match profiler with None -> () | _ -> Log.out "%a@." pp metrics
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

  let profiler (profiler : Enums.InterpProfiler.t) : (module Profiler.M) =
    match profiler with
    | None -> (module Profiler.Disable : Profiler.M)
    | Time -> (module Profiler.Time : Profiler.M)
    | Full -> (module Profiler.Full : Profiler.M)

  let monitor () : (module Monitor.M) = (module Monitor.Default : Monitor.M)

  let intrument (instrument : Options.instrument) : (module Instrument.M) =
    let module Tracer = (val tracer instrument.tracer) in
    let module Debugger = (val debugger instrument.debugger) in
    let module Profiler = (val profiler instrument.profiler) in
    let module Monitor = (val monitor ()) in
    (module Instrument.Default (Tracer) (Debugger) (Profiler) (Monitor))
end

let interpret_partial (entry : Interpreter.entry) (config : Options.config)
  (prog : Prog.t) : Interpreter.result =
  let instrument = config.instrument in
  let module Instrument = (val InterpreterInstrument.intrument instrument) in
  let module Interpreter = Interpreter.M (Instrument) in
  Interpreter.Config.print_depth := config.print_depth;
  Interpreter.Config.resolve_exitval := config.resolve_exitval;
  Interpreter.Config.show_exitval := config.show_exitval;
  Interpreter.eval_prog entry prog

let interpret (entry : Interpreter.entry) (config : Options.config)
  (prog : Prog.t) : Interpreter.result Result.t =
  Result.esl_exec @@ fun () ->
  let result = interpret_partial entry config prog in
  let retval = result.retval in
  Log.debug "Sucessfuly evaluated program with return '%a'." Val.pp retval;
  Ok result

let interpret_cesl (entry : Interpreter.entry) (config : Options.config)
  (file : Fpath.t) : Interpreter.result Result.t =
  let* p = Cmd_compile.load file in
  interpret entry config p

let interpret_esl (entry : Interpreter.entry) (config : Options.config)
  (untyped : bool) (file : Fpath.t) : Interpreter.result Result.t =
  let* p = Cmd_compile.compile untyped file in
  interpret entry config p

let log_metrics (profiler : Enums.InterpProfiler.t)
  (result : Interpreter.result Result.t) : unit Result.t =
  let* result' = result in
  Ok (InterpreterMetrics.log profiler result'.metrics)

let run () (opts : Options.t) : unit Result.t =
  let valid_langs = Enums.Lang.valid_langs Options.langs opts.lang in
  let entry = { (Interpreter.entry_default ()) with main = opts.main } in
  log_metrics opts.config.instrument.profiler
  @@
  match Enums.Lang.resolve_file_lang valid_langs opts.input with
  | Some ESL -> interpret_esl entry opts.config opts.untyped opts.input
  | Some CESL -> interpret_cesl entry opts.config opts.input
  | Some CESLUnattached | _ ->
    let config = { opts.config with resolve_exitval = false } in
    interpret_cesl entry config opts.input
