type t =
  | Void
  | Null
  | Flt of float
  | Int of int
  | Bool of bool
  | Str of string
  | Loc of Loc.t
  | List of t list
  | Arr of t array
  | Type of Type.t
  | Tuple of t list
  | Symbol of string
  | Curry of string * t list
  | Byte of int
  | Symbolic of Type.t * string
  | Unop of Operators.uopt * t
  | Binop of Operators.bopt * t * t

let rec of_val (v : Val.t) : t =
  match v with
  | Val.Void -> Void
  | Val.Null -> Null
  | Val.Flt f -> Flt f
  | Val.Int n -> Int n
  | Val.Bool b -> Bool b
  | Val.Str s -> Str s
  | Val.Loc l -> Loc l
  | Val.List l -> List (List.map of_val l)
  | Val.Arr a -> Arr (Array.map of_val a)
  | Val.Type t -> Type t
  | Val.Tuple t -> Tuple (List.map of_val t)
  | Val.Symbol s -> Symbol s
  | Val.Curry (c, a) -> Curry (c, List.map of_val a)
  | Val.Byte b -> Byte b

let rec is_symbolic (v : t) : bool =
  match v with
  | Symbolic (t, x) -> true
  | Unop (_, v) -> is_symbolic v
  | Binop (_, v1, v2) -> is_symbolic v1 || is_symbolic v2
  | List t | Tuple t | Curry (_, t) ->
      List.fold_left ( || ) false (List.map is_symbolic t)
  | Arr a -> Array.fold_left ( || ) false (Array.map is_symbolic a)
  | _ -> false

let is_special_number (s : string) : bool =
  List.mem s [ "nan"; "inf"; "-inf" ]
  || String.contains s 'e' || String.contains s 'E'

let add_final_dot (s : string) : string =
  if is_special_number s then s
  else
    try
      ignore (String.rindex s '.');
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
  | Symbolic (t, x) -> "Symbolic(" ^ Type.str t ^ ", " ^ x ^ ")"
  | Unop (op, v) -> Operators.str_of_unopt op ^ "(" ^ str v ^ ")"
  | Binop (op, v1, v2) ->
      let v1' = str v1 and v2' = str v2 in
      Operators.str_of_binopt op v1' v2'

let to_int (v : t) : int =
  match v with
  | Int i -> i
  | _ -> invalid_arg ("to_int: expects Int argument but got " ^ str v)

let to_float (v : t) : float =
  match v with
  | Flt f -> f
  | _ -> invalid_arg ("to_float: expects Flt argument but got " ^ str v)

let to_list (v : t) : t list =
  match v with
  | List l -> l
  | _ -> invalid_arg ("to_list: expects List argument but got" ^ str v)

let to_tuple (v : t) : t list =
  match v with
  | Tuple t -> t
  | _ -> invalid_arg ("to_tuple: expects Tuple argument but got" ^ str v)
