open Ecma_sl

type t =
  | None
  | Call
  | Step
  | Full
  | Core

let all = [ None; Call; Step; Full; Core ]

let pp (fmt : Fmt.t) (tracer : t) : unit =
  match tracer with
  | None -> Fmt.pp_str fmt "none"
  | Call -> Fmt.pp_str fmt "call"
  | Step -> Fmt.pp_str fmt "step"
  | Full -> Fmt.pp_str fmt "full"
  | Core -> Fmt.pp_str fmt "core"

let str (tracer : t) : string = Fmt.asprintf "%a" pp tracer

let args (tracers : t list) : (string * t) list =
  List.map (fun tracer -> (str tracer, tracer)) tracers
