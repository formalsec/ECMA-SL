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
  | Flt v    -> string_of_float v
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

let rec to_json (v : t): string =
  match v with
  | Flt v    ->  Printf.sprintf "{ \"type\" : \"float\", \"value\" : %f }" (string_of_float v)
  | Int v    ->  Printf.sprintf "{ \"type\" : \"int\", \"value\" : %d }" (string_of_int v)
  | Bool v   ->  Printf.sprintf "{ \"type\" : \"boolean\", \"value\" : %s }" (str v)
  | Str v    ->  Printf.sprintf "{ \"type\" : \"string\", \"value\" : %s }" v
  | Loc v    ->  Printf.sprintf "{ \"type\" : \"location\", \"value\" : %s }" v
  | List vs  ->  Printf.sprintf "{ \"type\" : \"list\", \"value\" : [ %s ] }" (String.concat ", " (List.map to_json vs))
  | Type v   ->  Printf.sprintf "{ \"type\" : \"type\", \"value\" : %s }" (Type.str v)
  | Tuple vs ->  Printf.sprintf "{ \"type\" : \"tuple\", \"value\" : [ %s ] }" (String.concat ", " (List.map to_json vs))
  | Void     ->  Printf.sprintf "{ \"type\" : \"void\" }"
  | Null     ->  Printf.sprintf "{ \"type\" : \"null\" }"
  | Symbol s ->  Printf.sprintf "{ \"type\" : \"symbol\", \"value\" : %s }" s
