open EslCore
open Source

type t = t' Source.phrase

and t' =
  | AnyType
  | UnknownType
  | NeverType
  | UndefinedType
  | NullType
  | VoidType
  | IntType
  | FloatType
  | StringType
  | BooleanType
  | SymbolType
  | LiteralType of tliteral
  | ObjectType of tobject
  | ListType of t
  | TupleType of t list
  | UnionType of t list
  | SigmaType of Id.t * t list
  | UserDefinedType of Id.t'

and tliteral =
  | IntegerLit of int
  | FloatLit of float
  | BooleanLit of bool
  | StringLit of string
  | SymbolLit of string

and tobject =
  { kind : tobjkind
  ; flds : (Id.t', tobjfld) Hashtbl.t
  ; smry : (Id.t * t) option
  }

and tobjkind =
  | ObjLit
  | ObjSto

and tobjfld = Id.t * t * tfldstyle

and tfldstyle =
  | FldReq
  | FldOpt

let tliteral_to_val (lt : tliteral) : Val.t =
  match lt with
  | IntegerLit i -> Val.Int i
  | FloatLit f -> Val.Flt f
  | StringLit s -> Val.Str s
  | BooleanLit b -> Val.Bool b
  | SymbolLit s -> Val.Symbol s

let rec equal (t1 : t) (t2 : t) : bool =
  let tsmry_get smry = Option.map (fun (_, tsmry) -> tsmry.it) smry in
  let tfld_equal (fn1, ft1, fs1) (fn2, ft2, fs2) =
    fn1.it = fn2.it && equal ft1 ft2 && fs1 = fs2
  in
  match (t1.it, t2.it) with
  | (AnyType, AnyType) -> true
  | (UnknownType, UnknownType) -> true
  | (NeverType, NeverType) -> true
  | (UndefinedType, UndefinedType) -> true
  | (NullType, NullType) -> true
  | (VoidType, VoidType) -> true
  | (IntType, IntType) -> true
  | (FloatType, FloatType) -> true
  | (StringType, StringType) -> true
  | (BooleanType, BooleanType) -> true
  | (SymbolType, SymbolType) -> true
  | (LiteralType lt1, LiteralType lt2) ->
    Val.equal (tliteral_to_val lt1) (tliteral_to_val lt2)
  | (ObjectType tobj1, ObjectType tobj2) ->
    let tflds1 = Hashtbl.to_seq_values tobj1.flds in
    let tflds2 = Hashtbl.to_seq_values tobj2.flds in
    tobj1.kind = tobj2.kind
    && Seq.length tflds1 == Seq.length tflds2
    && Seq.for_all (fun tfld1 -> Seq.exists (tfld_equal tfld1) tflds2) tflds1
    && tsmry_get tobj1.smry = tsmry_get tobj2.smry
  | (ListType t1, ListType t2) -> equal t1 t2
  | (TupleType ts1, TupleType ts2) -> List.equal equal ts1 ts2
  | (UnionType ts1, UnionType ts2) -> List.equal equal ts1 ts2
  | (SigmaType (dsc1, ts1), SigmaType (dsc2, ts2)) ->
    dsc1.it = dsc2.it && List.equal equal ts1 ts2
  | (UserDefinedType tvar1, UserDefinedType tvar2) -> tvar1 = tvar2
  | _ -> false

let pp_tobjfld (fmt : Fmt.t) ((fn, ft, fs) : tobjfld) : unit =
  let str_opt = function FldOpt -> "?" | _ -> "" in
  Fmt.fprintf fmt "%a%s: %a" Id.pp fn (str_opt fs) pp ft

let rec pp (fmt : Fmt.t) (t : t) : unit =
  let open Fmt in
  match t.it with
  | AnyType -> pp_str fmt "any"
  | UnknownType -> pp_str fmt "unknown"
  | NeverType -> pp_str fmt "never"
  | UndefinedType -> pp_str fmt "undefined"
  | NullType -> pp_str fmt "null"
  | VoidType -> pp_str fmt "void"
  | IntType -> pp_str fmt "int"
  | FloatType -> pp_str fmt "float"
  | StringType -> pp_str fmt "string"
  | BooleanType -> pp_str fmt "boolean"
  | SymbolType -> pp_str fmt "symbol"
  | LiteralType tl -> Val.pp fmt (tliteral_to_val tl)
  | ObjectType { flds; smry; _ } when Hashtbl.length flds = 0 ->
    let pp_smry fmt (_, tsmry) = fprintf fmt " *: %a " pp tsmry in
    fprintf fmt "{%a}" (pp_opt pp_smry) smry
  | ObjectType { flds; smry; _ } ->
    let pp_tfld fmt (_, tfld) = pp_tobjfld fmt tfld in
    let pp_smry fmt (_, tsmry) = fprintf fmt ", *: %a" pp tsmry in
    fprintf fmt "{ %a%a }" (pp_hashtbl ", " pp_tfld) flds (pp_opt pp_smry) smry
  | ListType t' -> fprintf fmt "%a[]" pp t'
  | TupleType ts -> fprintf fmt "(%a)" (pp_lst " * " pp) ts
  | UnionType ts -> fprintf fmt "(%a)" (pp_lst " | " pp) ts
  | SigmaType (dsc, ts) ->
    fprintf fmt "sigma [%a] | %a" Id.pp dsc (pp_lst " | " pp) ts
  | UserDefinedType tvar -> pp_str fmt tvar

let str (t : t) : string = Fmt.asprintf "%a" pp t

let tannot_pp (fmt : Fmt.t) (t : t option) =
  Fmt.(fprintf fmt "%a" (pp_opt (fun fmt t -> fprintf fmt ": %a" pp t)) t)

module TDef = struct
  type tval = t

  type t =
    { name : Id.t
    ; tval : tval
    }

  let create (name : Id.t) (tval : tval) : t = { name; tval }
  let name (tdef : t) : Id.t = tdef.name
  let name' (tdef : t) : Id.t' = tdef.name.it
  let tval (tdef : t) : tval = tdef.tval

  let pp (fmt : Fmt.t) (tdef : t) : unit =
    let { name = tn; tval = tv } = tdef in
    Fmt.fprintf fmt "typedef %a := %a" Id.pp tn pp tv

  let str (tdef : t) : string = Fmt.asprintf "%a" pp tdef
end
