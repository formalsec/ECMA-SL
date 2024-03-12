open EslCore

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
  let to_arg = function
    | None -> ("none", None)
    | Warn -> ("warn", Warn)
    | Full -> ("full", Full)
  in
  List.map to_arg levels

let value (level : t) : int =
  match level with None -> 0 | Warn -> 1 | Full -> 2
