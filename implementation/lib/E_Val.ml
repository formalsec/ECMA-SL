type t =
  | Flt of float
  | Int of int
  | Str of string
  | Bool of bool

let str (v : t) : string = match v with
  | Flt v   -> string_of_float v
  | Int v   -> string_of_int v
  | Str v   -> "\"" ^ v ^ "\""
  | Bool v  -> string_of_bool v
