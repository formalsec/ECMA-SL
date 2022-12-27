type t =
  | Flt of float
  | Int of int
  | Bool of bool
  | Str of string
  | Loc of Loc.t
  | List of t list
  | Arr of t array
  | Type of Type.t
  | Tuple of t list
  | Void
  | Null
  | Symbol of string
  | Curry of string * t list
  | Byte of int
  | Symbolic of Type.t

let is_special_number (s : string) : bool =
  List.mem s [ "nan"; "inf"; "-inf" ]
  || String.contains s 'e' || String.contains s 'E'

let add_final_dot (s : string) : string =
  if is_special_number s then s
  else
    try
      let _ = String.rindex s '.' in
      s
    with _ -> s ^ "."

let rec str ?(flt_with_dot = true) (v : t) : string =
  match v with
  | Flt v ->
      let s = Printf.sprintf "%.17g" v in
      if flt_with_dot then add_final_dot s else s
  | Int v -> string_of_int v
  | Bool v -> string_of_bool v
  | Str v -> Printf.sprintf "%S" v
  | Loc v -> Loc.str v
  | List vs -> "[" ^ String.concat ", " (List.map (str ~flt_with_dot) vs) ^ "]"
  | Arr vs ->
      "[|"
      ^ String.concat ", " (Array.to_list (Array.map (str ~flt_with_dot) vs))
      ^ "|]"
  | Type v -> Type.str v
  | Tuple vs -> "(" ^ String.concat ", " (List.map (str ~flt_with_dot) vs) ^ ")"
  | Void -> ""
  | Null -> "null"
  | Symbol s -> "'" ^ s
  | Curry (s, vs) ->
      Printf.sprintf "{\"%s\"}@(%s)" s
        (String.concat ", " (List.map (str ~flt_with_dot) vs))
  | Byte i -> string_of_int i
  | Symbolic t -> ""

