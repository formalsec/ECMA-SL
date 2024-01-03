type t =
  | Null
  | Void
  | Int of int
  | Flt of float
  | Str of string
  | Bool of bool
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
  | (Int i1, Int i2) -> Int.equal i1 i2
  | (Flt f1, Flt f2) -> Float.equal f1 f2
  | (Str s1, Str s2) -> String.equal s1 s2
  | (Bool b1, Bool b2) -> Bool.equal b1 b2
  | (Symbol s1, Symbol s2) -> String.equal s1 s2
  | (Loc l1, Loc l2) -> String.equal l1 l2
  | (Arr arr1, Arr arr2) ->
    if arr1 == arr2 then true else Array.for_all2 equal arr1 arr2
  | (List lst1, List lst2) -> List.equal equal lst1 lst2
  | (Tuple tup1, Tuple tup2) -> List.equal equal tup1 tup2
  | (Type t1, Type t2) -> Type.equal t1 t2
  | (Byte bt1, Byte bt2) -> Int.equal bt1 bt2
  | (Curry (fn1, fvs1), Curry (fn2, fvs2)) ->
    String.equal fn1 fn2 && List.equal equal fvs1 fvs2
  | _ -> v1 = v2

let rec copy (v : t) : t =
  match v with
  | Arr arr -> Arr (Array.copy arr)
  | List lst -> List (List.map copy lst)
  | Tuple tup -> Tuple (List.map copy tup)
  | Curry (fn, fvs) -> Curry (fn, List.map copy fvs)
  | _ -> v

let is_symbol (v : t) : bool = match v with Symbol _ -> true | _ -> false
let is_loc (v : t) : bool = match v with Loc _ -> true | _ -> false

let is_special_number (s : string) : bool =
  List.mem s [ "nan"; "inf"; "-inf" ]
  || String.contains s 'e'
  || String.contains s 'E'

let add_final_dot (s : string) : string =
  if is_special_number s then s
  else if String.contains s '.' then s
  else s ^ "."

let str_of_flt ?(flt_with_dot = true) (f : float) : string =
  let s = Printf.sprintf "%.17g" f in
  if flt_with_dot then add_final_dot s else s

let rec str ?(flt_with_dot = true) (v : t) : string =
  match v with
  | Null -> "null"
  | Void -> ""
  | Int i -> Int.to_string i
  | Flt f -> str_of_flt ~flt_with_dot f
  | Str s -> Printf.sprintf "%S" s
  | Bool v -> Bool.to_string v
  | Symbol s -> "'" ^ s
  | Loc l -> Loc.str l
  | Arr arr ->
    let vs' = Array.to_list arr in
    "[|" ^ String.concat ", " (List.map (str ~flt_with_dot) vs') ^ "|]"
  | List lst ->
    "[" ^ String.concat ", " (List.map (str ~flt_with_dot) lst) ^ "]"
  | Tuple tup ->
    "(" ^ String.concat ", " (List.map (str ~flt_with_dot) tup) ^ ")"
  | Type t -> Type.str t
  | Byte bt -> Int.to_string bt
  | Curry (fn, fvs) ->
    let vs_str = List.map (str ~flt_with_dot) fvs |> String.concat ", " in
    Printf.sprintf "{\"%s\"}@(%s)" fn vs_str

let rec to_json (v : t) : string =
  let _lst_to_json lst = List.map to_json lst |> String.concat ", " in
  match v with
  | Null -> Printf.sprintf "{ \"type\" : \"null\" }"
  | Void -> Printf.sprintf "{ \"type\" : \"void\" }"
  | Int i -> Printf.sprintf "{ \"type\" : \"int\", \"value\" : %d }" i
  | Flt f ->
    Printf.sprintf "{ \"type\" : \"float\", \"value\" : %s }" (str_of_flt f)
  | Str s -> Printf.sprintf "{ \"type\" : \"string\", \"value\" : \"%s\" }" s
  | Bool b -> Printf.sprintf "{ \"type\" : \"boolean\", \"value\" : %b }" b
  | Symbol s -> Printf.sprintf "{ \"type\" : \"symbol\", \"value\" : \"%s\" }" s
  | Loc l -> Printf.sprintf "{ \"type\" : \"location\", \"value\" : %s }" l
  | Arr arr ->
    let arr_str = Array.to_list arr |> _lst_to_json in
    Printf.sprintf "{ \"type\" : \"array\", \"value\" : [ %s ] }" arr_str
  | List lst ->
    let lst_str = _lst_to_json lst in
    Printf.sprintf "{ \"type\" : \"list\", \"value\" : [ %s ] }" lst_str
  | Tuple tup ->
    let tup_str = _lst_to_json tup in
    Printf.sprintf "{ \"type\" : \"tuple\", \"value\" : [ %s ] }" tup_str
  | Type t ->
    Printf.sprintf "{ \"type\" : \"type\", \"value\" : %s }" (Type.str t)
  | Byte bt -> Printf.sprintf "{ \"type\" : \"byte\", \"value\" : \"%d\" }" bt
  | Curry (fn, fvs) ->
    let vs_str = _lst_to_json fvs in
    Printf.sprintf
      "{ \"type\" : \"curry\", \"fun\" : \"%s\", \"args\" : [ %s ] }" fn vs_str
