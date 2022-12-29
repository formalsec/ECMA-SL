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
  | Unop of Oper.uopt * t
  | Binop of Oper.bopt * t * t

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
  | Symbolic (_, _) -> ""
  | Unop (_, _) -> ""
  | Binop (_, _, _) -> ""

let neg v = match v with Flt v -> Flt (-.v) | Int v -> Int (-v) | _ -> v
let not v = match v with Bool v -> Bool (not v) | _ -> Unop (Oper.Not, v)
let is_NaN v = match v with Flt v -> Bool (Float.is_nan v) | _ -> Bool false

let times v1 v2 =
  match (v1, v2) with
  | Flt v1, Flt v2 -> Flt (v1 *. v2)
  | Int v1, Int v2 -> Int (v1 * v2)
  | _ -> Binop (Oper.Times, v1, v2)

let gt v1 v2 =
  match (v1, v2) with
  | Flt v1', Flt v2' -> Bool (v1' > v2')
  | Int v1', Int v2' -> Bool (v1' > v2')
  | _ -> Binop (Oper.Gt, v1, v2)

let lt v1 v2 =
  match (v1, v2) with
  | Flt v1', Flt v2' -> Bool (v1' < v2')
  | Int v1', Int v2' -> Bool (v1' < v2')
  | _ -> Binop (Oper.Lt, v1, v2)

let gte v1 v2 =
  match (v1, v2) with
  | Flt v1', Flt v2' -> Bool (v1' >= v2')
  | Int v1', Int v2' -> Bool (v1' >= v2')
  | _ -> Binop (Oper.Egt, v1, v2)

let lte v1 v2 =
  match (v1, v2) with
  | Flt v1', Flt v2' -> Bool (v1' <= v2')
  | Int v1', Int v2' -> Bool (v1' <= v2')
  | _ -> Binop (Oper.Elt, v1, v2)
