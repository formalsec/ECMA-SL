open Smtml
open EslBase
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
  | LiteralType of tlitkind * tliteral
  | ObjectType of tobject
  | ListType of t
  | TupleType of t list
  | UnionType of t list
  | SigmaType of Id.t * t list
  | UserDefinedType of Id.t'

and tlitkind =
  | LitWeak
  | LitStrong

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

let resolve_topt (t : t option) : t =
  match t with Some t' -> t' | None -> AnyType @> no_region

let tliteral_to_val (lt : tliteral) : Value.t =
  match lt with
  | IntegerLit i -> Value.Int i
  | FloatLit f -> Value.Real f
  | StringLit s -> Value.Str s
  | BooleanLit b -> if b then Value.True else Value.False
  | SymbolLit s -> Value.App (`Op "symbol", [Value.Str s])

let tliteral_to_wide (lt : tliteral) : t' =
  match lt with
  | IntegerLit _ -> IntType
  | FloatLit _ -> FloatType
  | StringLit _ -> StringType
  | BooleanLit _ -> BooleanType
  | SymbolLit _ -> SymbolType

let rec equal (t1 : t) (t2 : t) : bool =
  let tsmry_get smry = Option.map (fun (_, tsmry) -> tsmry.it) smry in
  let tfld_equal (fn1, ft1, fs1) (fn2, ft2, fs2) =
    String.equal fn1.it fn2.it && equal ft1 ft2 && fs1 == fs2
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
  | (LiteralType (_, lt1), LiteralType (_, lt2)) ->
    Value.equal (tliteral_to_val lt1) (tliteral_to_val lt2)
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

let pp_tobjfld (ppf : Fmt.t) ((fn, ft, fs) : tobjfld) : unit =
  let str_opt = function FldOpt -> "?" | _ -> "" in
  Fmt.format ppf "%a%s: %a" Id.pp fn (str_opt fs) pp ft

let rec pp (ppf : Fmt.t) (t : t) : unit =
  let open Fmt in
  match t.it with
  | AnyType -> pp_str ppf "any"
  | UnknownType -> pp_str ppf "unknown"
  | NeverType -> pp_str ppf "never"
  | UndefinedType -> pp_str ppf "undefined"
  | NullType -> pp_str ppf "null"
  | VoidType -> pp_str ppf "void"
  | IntType -> pp_str ppf "int"
  | FloatType -> pp_str ppf "float"
  | StringType -> pp_str ppf "string"
  | BooleanType -> pp_str ppf "boolean"
  | SymbolType -> pp_str ppf "symbol"
  | LiteralType (_, tl) -> Value.pp ppf (tliteral_to_val tl)
  | ObjectType { flds; smry; _ } when Hashtbl.length flds = 0 ->
    let pp_smry ppf (_, tsmry) = format ppf " *: %a " pp tsmry in
    format ppf "{%a}" (pp_opt pp_smry) smry
  | ObjectType { flds; smry; _ } ->
    let pp_tfld ppf (_, tfld) = pp_tobjfld ppf tfld in
    let pp_smry ppf (_, tsmry) = format ppf ", *: %a" pp tsmry in
    format ppf "{ %a%a }" (pp_hashtbl !>", " pp_tfld) flds (pp_opt pp_smry) smry
  | ListType t' -> format ppf "%a[]" pp t'
  | TupleType ts -> format ppf "(%a)" (pp_lst !>" * " pp) ts
  | UnionType ts -> format ppf "(%a)" (pp_lst !>" | " pp) ts
  | SigmaType (dsc, ts) ->
    format ppf "sigma [%a] | %a" Id.pp dsc (pp_lst !>" | " pp) ts
  | UserDefinedType tvar -> pp_str ppf tvar

let str (t : t) : string = Fmt.str "%a" pp t

let tannot_pp (ppf : Fmt.t) (t : t option) =
  Fmt.(format ppf "%a" (pp_opt (fun ppf t -> format ppf ": %a" pp t)) t)

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

  let pp (ppf : Fmt.t) (tdef : t) : unit =
    let { name = tn; tval = tv } = tdef in
    Fmt.format ppf "typedef %a := %a" Id.pp tn pp tv

  let str (tdef : t) : string = Fmt.str "%a" pp tdef
end
