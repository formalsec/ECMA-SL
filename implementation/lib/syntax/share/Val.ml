type t =
  | Flt    of float
  | Int    of int
  | Bool   of bool
  | Str    of string
  | Loc    of Loc.t
  | List   of t list
  | Type   of Type.t
  | Tuple  of t list
  | Void
  | Null
  | Symbol of string

let rec str (v : t) : string = match v with
  | Flt v    -> Printf.sprintf "%.12g" v
  | Int v    -> string_of_int v
  | Bool v   -> string_of_bool v
  | Str v    -> "\"" ^ v ^ "\""
  | Loc v    -> Loc.str v
  | List vs  -> "[" ^ (String.concat ", " (List.map str vs)) ^ "]"
  | Type v   -> Type.str v
  | Tuple vs -> "(" ^ (String.concat ", " (List.map str vs)) ^ ")"
  | Void     -> ""
  | Null     -> "null"
  | Symbol s -> s
