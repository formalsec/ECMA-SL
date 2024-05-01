open Smtml
open EslBase

type t = t' Source.phrase

and t' =
  | Val of Value.t
  | Var of Id.t'
  | UnOpt of Operator.unopt * t
  | BinOpt of Operator.binopt * t * t
  | TriOpt of Operator.triopt * t * t * t
  | NOpt of Operator.nopt * t list
  | Curry of t * t list
  | Symbolic of Type.t * t

let is_special_number (s : string) : bool =
  List.mem s [ "nan"; "inf"; "-inf" ]
  || String.contains s 'e'
  || String.contains s 'E'

let float_str (f : float) : string =
  let f_str = Printf.sprintf "%.17g" f in
  if is_special_number f_str || String.contains f_str '.' then f_str
  else f_str ^ ".0"

let pp_val_custom_inner (pp_inner_val : Fmt.t -> Smtml.Value.t -> unit) (ppf : Fmt.t) (v : Smtml.Value.t) :
  unit =
  let open Fmt in
  match v with
  | App (`Op "null", []) -> format ppf "null"
  | App (`Op "void", []) -> ()
  | Int i -> format ppf "%i" i
  | Real f -> format ppf "%s" (float_str f)
  | Str s -> format ppf "%S" s
  | True -> format ppf "true"
  | False -> format ppf "false"
  | App (`Op "symbol", [Str s]) -> format ppf "'%s" s
  | App (`Op "loc", [Int l]) -> Loc.pp ppf l
  | List lst -> format ppf "[%a]" (pp_lst !>", " pp_inner_val) lst
  | App (`Op "NullType" , []) -> format ppf "__$Null" 
  | App (`Op "IntType" , []) -> format ppf "__$Int" 
  | App (`Op "RealType" , []) -> format ppf "__$Flt" 
  | App (`Op "StrType" , []) -> format ppf "__$Str" 
  | App (`Op "BoolType" , []) -> format ppf "__$Bool" 
  | App (`Op "SymbolType" , []) -> format ppf "__$Symbol" 
  | App (`Op "LocType" , []) -> format ppf "__$Obj" 
  | App (`Op "ListType" , []) -> format ppf "__$List" 
  | App (`Op "CurryType" , []) -> format ppf "__$Curry" 
  | App (`Op fn, fvs) ->
    format ppf "{%S}@(%a)" fn (pp_lst !>", " pp_inner_val) fvs
  | _ -> failwith "Val.pp_val_custom_inner: unexpected case"

let rec pp_val (ppf : Fmt.t) (v : Value.t) : unit = pp_val_custom_inner pp_val ppf v

let rec pp (ppf : Fmt.t) (e : t) : unit =
  let open Fmt in
  match e.it with
  | Val v -> pp_val ppf v
  | Var x -> pp_str ppf x
  | UnOpt (op, e') -> Operator.pp_of_unopt pp ppf (op, e')
  | BinOpt (op, e1, e2) -> Operator.pp_of_binopt pp ppf (op, e1, e2)
  | TriOpt (op, e1, e2, e3) -> Operator.pp_of_triopt pp ppf (op, e1, e2, e3)
  | NOpt (op, es) -> Operator.pp_of_nopt pp ppf (op, es)
  | Curry (fe, es) -> format ppf "{%a}@(%a)" pp fe (pp_lst !>", " pp) es
  | Symbolic (t, e') -> format ppf "se_mk_symbolic(%a, %a)" Type.pp t pp e'

let str (e : t) : string = Fmt.str "%a" pp e
let isvoid (e : t) : bool = match e.it with Val App (`Op "void", []) -> true | _ -> false

let rec vars_in_expr (e : t) : Id.t' list =
  let vars_in_lst lst = List.map vars_in_expr lst |> List.concat in
  match e.it with
  | Var x -> [ x ]
  | UnOpt (_, e') -> vars_in_lst [ e' ]
  | BinOpt (_, e1, e2) -> vars_in_lst [ e1; e2 ]
  | TriOpt (_, e1, e2, e3) -> vars_in_lst [ e1; e2; e3 ]
  | NOpt (_, es) -> vars_in_lst es
  | Curry (fe, es) -> vars_in_lst (fe :: es)
  | _ -> []
