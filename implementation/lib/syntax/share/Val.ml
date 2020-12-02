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
  | Undef
  | Symbol of string


let is_special_number (s : string) : bool =
  s = "nan" || s = "inf" || String.contains s 'e' ||  String.contains s 'E'

let add_final_dot (s : string) : string =
  if is_special_number s
  then s
  else
    try
      let _ = String.rindex s '.' in s
    with _ -> s ^ "."

let rec str ?(flt_with_dot=true) (v : t) : string = match v with
  | Flt v    ->
    let s = Printf.sprintf "%.17g" v in
    if flt_with_dot
    then add_final_dot s
    else s
  | Int v    -> string_of_int v
  | Bool v   -> string_of_bool v
  | Str v    -> Printf.sprintf "\"%s\"" v
  | Loc v    -> Loc.str v
  | List vs  -> "[" ^ (String.concat ", " (List.map str vs)) ^ "]"
  | Type v   -> Type.str v
  | Tuple vs -> "(" ^ (String.concat ", " (List.map str vs)) ^ ")"
  | Void     -> ""
  | Null     -> "null"
  | Undef    -> "undefined"
  | Symbol s -> s

let rec to_json (v : t): string =
  match v with
  | Flt v    ->  Printf.sprintf "{ \"type\" : \"float\", \"value\" : %s }" (Printf.sprintf "%.12g" v)
  | Int v    ->  Printf.sprintf "{ \"type\" : \"int\", \"value\" : %s }" (string_of_int v)
  | Bool v   ->  Printf.sprintf "{ \"type\" : \"boolean\", \"value\" : %s }" (string_of_bool v)
  | Str v    ->  Printf.sprintf "{ \"type\" : \"string\", \"value\" : \"%s\" }" v
  | Loc v    ->  Printf.sprintf "{ \"type\" : \"location\", \"value\" : %s }" v
  | List vs  ->  Printf.sprintf "{ \"type\" : \"list\", \"value\" : [ %s ] }" (String.concat ", " (List.map to_json vs))
  | Type v   ->  Printf.sprintf "{ \"type\" : \"type\", \"value\" : %s }" (Type.str v)
  | Tuple vs ->  Printf.sprintf "{ \"type\" : \"tuple\", \"value\" : [ %s ] }" (String.concat ", " (List.map to_json vs))
  | Void     ->  Printf.sprintf "{ \"type\" : \"void\" }"
  | Null     ->  Printf.sprintf "{ \"type\" : \"null\" }"
  | Undef    ->  Printf.sprintf "{ \"type\" : \"undefined\" }"
  | Symbol s ->  Printf.sprintf "{ \"type\" : \"symbol\", \"value\" : \"%s\" }" s
