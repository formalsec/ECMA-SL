open Core

type t =
  | Null
  | Void
  | Int of int
  | Flt of float
  | Bool of bool
  | Str of string
  | Symbol of string
  | Loc of Loc.t
  | Arr of t array
  | List of t list
  | Tuple of t list
  | Byte of int
  | Type of Type.t
  | Curry of string * t list

let rec equal (v1 : t) (v2 : t) : bool =
  match (v1, v2) with
  | (Null, Null) -> true
  | (Void, Void) -> true
  | (Int i1, Int i2) -> Int.(i1 = i2)
  | (Flt f1, Flt f2) -> Float.(f1 = f2)
  | (Bool b1, Bool b2) -> Bool.(b1 = b2)
  | (Str s1, Str s2) -> String.equal s1 s2
  | (Loc l1, Loc l2) -> String.equal l1 l2
  | (Symbol s1, Symbol s2) -> String.equal s1 s2
  | (Arr a1, Arr a2) -> Array.equal equal a1 a2
  | (List l1, List l2) -> List.equal equal l1 l2
  | (Tuple t1, Tuple t2) -> List.equal equal t1 t2
  | (Type t1, Type t2) -> Type.(t1 = t2)
  | (Byte bt1, Byte bt2) -> Int.(bt1 = bt2)
  | (Curry (x1, vs1), Curry (x2, vs2)) ->
    String.equal x1 x2 && List.equal equal vs1 vs2
  | _ -> false

let rec copy (v : t) : t =
  match v with
  | Arr x -> Arr (Array.copy x)
  | List x -> List (List.map ~f:copy x)
  | Tuple x -> Tuple (List.map ~f:copy x)
  | Curry (x, vs) -> Curry (x, List.map ~f:copy vs)
  | x -> x

let is_symbol (v : t) : bool =
  match v with
  | Symbol _ -> true
  | _ -> false

let is_loc (v : t) : bool =
  match v with
  | Loc _ -> true
  | _ -> false

let is_special_number (s : string) : bool =
  List.mem ~equal:String.equal [ "nan"; "inf"; "-inf" ] s
  || String.contains s 'e'
  || String.contains s 'E'

let add_final_dot (s : string) : string =
  if is_special_number s then s
  else if String.contains s '.' then s
  else s ^ "."

let rec str ?(flt_with_dot = true) (v : t) : string =
  match v with
  | Null -> "null"
  | Void -> ""
  | Int v -> Int.to_string v
  | Flt v ->
    let s = Printf.sprintf "%.17g" v in
    if flt_with_dot then add_final_dot s else s
  | Bool v -> Bool.to_string v
  | Str v -> Printf.sprintf "%S" v
  | Loc v -> Loc.str v
  | Symbol s -> "'" ^ s
  | Arr vs ->
    let vs' = Array.to_list vs in
    "[|" ^ String.concat ~sep:", " (List.map ~f:(str ~flt_with_dot) vs') ^ "|]"
  | List vs ->
    "[" ^ String.concat ~sep:", " (List.map ~f:(str ~flt_with_dot) vs) ^ "]"
  | Tuple vs ->
    "(" ^ String.concat ~sep:", " (List.map ~f:(str ~flt_with_dot) vs) ^ ")"
  | Type v -> Type.str v
  | Byte i -> Int.to_string i
  | Curry (s, vs) ->
    Printf.sprintf "{\"%s\"}@(%s)" s
      (String.concat ~sep:", " (List.map ~f:(str ~flt_with_dot) vs))

let rec to_json (v : t) : string =
  match v with
  | Null -> Printf.sprintf "{ \"type\" : \"null\" }"
  | Void -> Printf.sprintf "{ \"type\" : \"void\" }"
  | Int v ->
    Printf.sprintf "{ \"type\" : \"int\", \"value\" : %s }" (Int.to_string v)
  | Flt v ->
    Printf.sprintf "{ \"type\" : \"float\", \"value\" : %s }"
      (Printf.sprintf "%.12g" v)
  | Bool v ->
    Printf.sprintf "{ \"type\" : \"boolean\", \"value\" : %s }"
      (Bool.to_string v)
  | Str v -> Printf.sprintf "{ \"type\" : \"string\", \"value\" : \"%s\" }" v
  | Symbol s -> Printf.sprintf "{ \"type\" : \"symbol\", \"value\" : \"%s\" }" s
  | Loc v -> Printf.sprintf "{ \"type\" : \"location\", \"value\" : %s }" v
  | Arr vs ->
    Printf.sprintf "{ \"type\" : \"array\", \"value\" : [| %s |] }"
      (String.concat ~sep:", "
         (Array.to_list (Array.map ~f:(str ~flt_with_dot:true) vs)) )
  | List vs ->
    Printf.sprintf "{ \"type\" : \"list\", \"value\" : [ %s ] }"
      (String.concat ~sep:", " (List.map ~f:to_json vs))
  | Tuple vs ->
    Printf.sprintf "{ \"type\" : \"tuple\", \"value\" : [ %s ] }"
      (String.concat ~sep:", " (List.map ~f:to_json vs))
  | Type v ->
    Printf.sprintf "{ \"type\" : \"type\", \"value\" : %s }" (Type.str v)
  | Byte i ->
    Printf.sprintf "{ \"type\" : \"byte\", \"value\" : \"%s\" }"
      (Int.to_string i)
  | Curry (s, vs) ->
    Printf.sprintf
      "{ \"type\" : \"curry\", \"fun\" : \"%s\", \"args\" : [ %s ] }" s
      (String.concat ~sep:", " (List.map ~f:to_json vs))
