open Ecma_sl

module Options = struct
  let langs : Lang.t list = Lang.[ Auto; ESL; CESL; CESLUnattached ]
  let trace = ref Interp_tracer.None
  let trace_loc = ref false
  let trace_depth = ref 0
  let debugger = ref false

  type t =
    { input : Fpath.t
    ; lang : Lang.t
    ; main : string
    ; show_exitval : bool
    }

  let set_options input lang trace' trace_loc' trace_depth' debugger' main
    show_exitval untyped' =
    Cmd_compile.Options.untyped := untyped';
    trace := trace';
    trace_loc := trace_loc';
    trace_depth := trace_depth';
    debugger := debugger';
    { input; lang; main; show_exitval }
end

module InterpreterInstrument = struct
  let tracer () : (module Tracer.M) =
    Tracer.Config.trace_loc := !Options.trace_loc;
    Tracer.Config.trace_depth :=
      if !Options.trace_depth > 0 then Some !Options.trace_depth else None;
    match !Options.trace with
    | None -> (module Tracer.Disable : Tracer.M)
    | Call -> (module Tracer.Call : Tracer.M)
    | Step -> (module Tracer.Step : Tracer.M)
    | Full -> (module Tracer.Full : Tracer.M)
    | Core -> (module Tracer.Core : Tracer.M)

  let debugger () : (module Debugger.M) =
    match !Options.debugger with
    | true -> (module Debugger.Enable : Debugger.M)
    | false -> (module Debugger.Disable : Debugger.M)

  let monitor () : (module Monitor.M) = (module Monitor.Default : Monitor.M)

  let intrument () : (module Instrument.M) =
    let module Tracer = (val tracer ()) in
    let module Debugger = (val debugger ()) in
    let module Monitor = (val monitor ()) in
    (module Instrument.Default (Tracer) (Debugger) (Monitor))
end

let interpret_partial (entry : Interpreter.EntryPoint.t) (prog : Prog.t) :
  Val.t * Val.t Heap.t =
  let module Instrument = (val InterpreterInstrument.intrument ()) in
  let module Interpreter = Interpreter.M (Instrument) in
  Interpreter.eval_partial entry prog

let interpret (entry : Interpreter.EntryPoint.t) (prog : Prog.t) : Val.t =
  let retval = fst (interpret_partial entry prog) in
  Log.debug "Sucessfuly evaluated program with return '%a'." Val.pp retval;
  retval

let interpret_cesl (entry : Interpreter.EntryPoint.t) (file : Fpath.t) : Val.t =
  let file' = Fpath.to_string file in
  Parsing.load_file file' |> Parsing.parse_prog ~file:file' |> interpret entry

let interpret_esl (entry : Interpreter.EntryPoint.t) (file : Fpath.t) : Val.t =
  Cmd_compile.compile file |> interpret entry

let process_exitval (show_exitval : bool) (exitval : Val.t) : unit =
  if show_exitval then Log.out "» exit value: %a@." Val.pp exitval

let run (opts : Options.t) : unit =
  let valid_langs = Lang.valid_langs Options.langs opts.lang in
  let config = { Interpreter.EntryPoint.default with main = opts.main } in
  process_exitval opts.show_exitval
  @@
  match Lang.resolve_file_lang valid_langs opts.input with
  | Some ESL -> interpret_esl config opts.input
  | Some CESL -> interpret_cesl config opts.input
  | Some CESLUnattached | _ ->
    interpret_cesl { config with resolve_exitval = false } opts.input

let main () (opts : Options.t) : int = Cmd.eval_cmd (fun () -> run opts)
