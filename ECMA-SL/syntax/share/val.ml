open EslBase

type t =
  | Null
  | Void
  | Int of int
  | Flt of (float[@unboxed])
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

let rec copy (v : t) : t =
  match v with
  | Arr arr -> Arr (Array.copy arr)
  | List lst -> List (List.map copy lst)
  | Tuple tup -> Tuple (List.map copy tup)
  | Curry (fn, fvs) -> Curry (fn, List.map copy fvs)
  | _ -> v

let rec equal (v1 : t) (v2 : t) : bool =
  match (v1, v2) with
  | (Null, Null) | (Void, Void) -> true
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
  | _ -> false

let type_of (v : t) : Type.t option =
  match v with
  | Void -> None
  | Null -> Some Type.NullType
  | Int _ -> Some Type.IntType
  | Flt _ -> Some Type.FltType
  | Bool _ -> Some Type.BoolType
  | Str _ -> Some Type.StrType
  | Loc _ -> Some Type.LocType
  | List _ -> Some Type.ListType
  | Arr _ -> Some Type.ArrayType
  | Tuple _ -> Some Type.TupleType
  | Curry _ -> Some Type.CurryType
  | Byte _ -> Some Type.IntType
  | Type _ -> Some Type.TypeType
  | Symbol _ -> Some Type.SymbolType

let is_special_number (s : string) : bool =
  List.mem s [ "nan"; "inf"; "-inf" ]
  || String.contains s 'e'
  || String.contains s 'E'

let float_str (f : float) : string =
  let f_str = Printf.sprintf "%.17g" f in
  if is_special_number f_str || String.contains f_str '.' then f_str
  else f_str ^ ".0"

let rec pp (fmt : Fmt.t) (v : t) : unit =
  let open Fmt in
  match v with
  | Null -> fprintf fmt "null"
  | Void -> ()
  | Int i -> fprintf fmt "%i" i
  | Flt f -> fprintf fmt "%s" (float_str f)
  | Str s -> fprintf fmt "%S" s
  | Bool b -> fprintf fmt "%b" b
  | Symbol s -> fprintf fmt "'%s" s
  | Loc l -> Loc.pp fmt l
  | Arr arr -> fprintf fmt "[|%a|]" (pp_arr ", " pp) arr
  | List lst -> fprintf fmt "[%a]" (pp_lst ", " pp) lst
  | Tuple tup -> fprintf fmt "(%a)" (pp_lst ", " pp) tup
  | Byte bt -> fprintf fmt "%i" bt
  | Type t -> Type.pp fmt t
  | Curry (fn, fvs) -> fprintf fmt "{%S}@(%a)" fn (pp_lst ", " pp) fvs

let str v = Fmt.asprintf "%a" pp v
