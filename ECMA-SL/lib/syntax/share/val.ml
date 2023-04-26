open Base

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

let rec equal (v1 : t) (v2 : t) : bool =
  match (v1, v2) with
  | Flt f1, Flt f2 -> Float.(f1 = f2)
  | Byte i1, Byte i2 | Int i1, Int i2 -> Int.(i1 = i2)
  | Bool b1, Bool b2 -> Bool.(b1 = b2)
  | Symbol s1, Symbol s2 | Str s1, Str s2 | Loc s1, Loc s2 -> String.equal s1 s2
  | Arr a1, Arr a2 -> Array.equal equal a1 a2
  | List l1, List l2 -> List.equal equal l1 l2
  | Type t1, Type t2 -> Type.(t1 = t2)
  | Tuple t1, Tuple t2 -> List.equal equal t1 t2
  | Void, Void | Null, Null -> true
  | Curry (x1, vs1), Curry (x2, vs2) ->
      String.equal x1 x2 && List.equal equal vs1 vs2
  | _ -> false

let is_symbol = function Symbol _ -> true | _ -> false
let is_loc = function Loc _ -> true | _ -> false

let is_special_number (s : string) : bool =
  List.mem [ "nan"; "inf"; "-inf" ] s ~equal:String.equal
  || String.contains s 'e' || String.contains s 'E'

let add_final_dot (s : string) : string =
  if is_special_number s then s
  else
    try
      let _ = String.rindex_exn s '.' in
      s
    with _ -> s ^ "."

let rec str ?(flt_with_dot = true) (v : t) : string =
  match v with
  | Flt v ->
      let s = Printf.sprintf "%.17g" v in
      if flt_with_dot then add_final_dot s else s
  | Int v -> Int.to_string v
  | Bool v -> Bool.to_string v
  | Str v -> Printf.sprintf "%S" v
  | Loc v -> Loc.str v
  | List vs ->
      "[" ^ String.concat ~sep:", " (List.map ~f:(str ~flt_with_dot) vs) ^ "]"
  | Arr vs ->
      "[|"
      ^ String.concat ~sep:", "
          (Array.to_list (Array.map ~f:(str ~flt_with_dot) vs))
      ^ "|]"
  | Type v -> Type.str v
  | Tuple vs ->
      "(" ^ String.concat ~sep:", " (List.map ~f:(str ~flt_with_dot) vs) ^ ")"
  | Void -> ""
  | Null -> "null"
  | Symbol s -> "'" ^ s
  | Curry (s, vs) ->
      Printf.sprintf "{\"%s\"}@(%s)" s
        (String.concat ~sep:", " (List.map ~f:(str ~flt_with_dot) vs))
  | Byte i -> Int.to_string i

let rec to_json (v : t) : string =
  match v with
  | Flt v ->
      Printf.sprintf "{ \"type\" : \"float\", \"value\" : %s }"
        (Printf.sprintf "%.12g" v)
  | Int v ->
      Printf.sprintf "{ \"type\" : \"int\", \"value\" : %s }" (Int.to_string v)
  | Bool v ->
      Printf.sprintf "{ \"type\" : \"boolean\", \"value\" : %s }"
        (Bool.to_string v)
  | Str v -> Printf.sprintf "{ \"type\" : \"string\", \"value\" : \"%s\" }" v
  | Loc v -> Printf.sprintf "{ \"type\" : \"location\", \"value\" : %s }" v
  | List vs ->
      Printf.sprintf "{ \"type\" : \"list\", \"value\" : [ %s ] }"
        (String.concat ~sep:", " (List.map ~f:to_json vs))
  | Arr vs ->
      Printf.sprintf "{ \"type\" : \"array\", \"value\" : [| %s |] }"
        (String.concat ~sep:", "
           (Array.to_list (Array.map ~f:(str ~flt_with_dot:true) vs)))
  | Type v ->
      Printf.sprintf "{ \"type\" : \"type\", \"value\" : %s }" (Type.str v)
  | Tuple vs ->
      Printf.sprintf "{ \"type\" : \"tuple\", \"value\" : [ %s ] }"
        (String.concat ~sep:", " (List.map ~f:to_json vs))
  | Void -> Printf.sprintf "{ \"type\" : \"void\" }"
  | Null -> Printf.sprintf "{ \"type\" : \"null\" }"
  | Symbol s -> Printf.sprintf "{ \"type\" : \"symbol\", \"value\" : \"%s\" }" s
  | Curry (s, vs) ->
      Printf.sprintf
        "{ \"type\" : \"curry\", \"fun\" : \"%s\", \"args\" : [ %s ] }" s
        (String.concat ~sep:", " (List.map ~f:to_json vs))
  | Byte i ->
      Printf.sprintf "{ \"type\" : \"byte\", \"value\" : \"%s\" }"
        (Int.to_string i)
