open Ecma_sl

type t =
  | None
  | Warn
  | Full

let all = [ None; Warn; Full ]

let pp (fmt : Fmt.t) (level : t) : unit =
  match level with
  | None -> Fmt.pp_str fmt "none"
  | Warn -> Fmt.pp_str fmt "warn"
  | Full -> Fmt.pp_str fmt "full"

let str (level : t) : string = Fmt.asprintf "%a" pp level

let args (levels : t list) : (string * t) list =
  List.map (fun level -> (str level, level)) levels

let value (level : t) : int =
  match level with None -> 0 | Warn -> 1 | Full -> 2
