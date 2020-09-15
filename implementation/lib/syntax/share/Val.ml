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
  | Flt v    ->  "{ \"type\" : \"number\" : " ^ string_of_float v ^ "}"
  | Int v    ->  "{ \"number\" : " ^ string_of_int v ^ "}"
  | Bool v   ->  "{ \"boolean\" : " ^ str v ^ "}" 
  | Str v    ->  "{ \"string\" : " ^ str v ^ "}" 
  | Loc v    ->  "{ \"location\" : " ^ str v ^ "}" 
  | List vs  ->  "[" ^ (String.concat ", " (List.map to_json vs)) ^ "]"
  | Type v   ->     .to_json v
  | Tuple vs ->  "(" ^ (String.concat ", " (List.map to_json vs)) ^ ")"
  | Void     ->  "{ \"void\" : " ^ str v ^ "}" 
  | Null     ->  "{ \"null\" : " ^ str v ^ "}" 
  | Symbol s ->  "{ \"void\" : " ^ s ^ "}"
