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
  ; flds : (string, tobjfld) Hashtbl.t
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
  | ObjectType tobj ->
    let pp_tfld fmt (_, tfld) = pp_tobjfld fmt tfld in
    if Hashtbl.length tobj.flds = 0 then pp_str fmt "{}"
    else fprintf fmt "{ %a }" (pp_hashtbl ", " pp_tfld) tobj.flds
  | ListType t' -> fprintf fmt "%a[]" pp t'
  | TupleType ts -> fprintf fmt "(%a)" (pp_lst " * " pp) ts
  | UnionType ts -> fprintf fmt "(%a)" (pp_lst " | " pp) ts
  | SigmaType (dsc, ts) ->
    fprintf fmt "sigma [%a] | %a" Id.pp dsc (pp_lst " | " pp) ts
  | UserDefinedType tvar -> pp_str fmt tvar

and pp_tobjfld (fmt : Fmt.t) ((fn, ft, fs) : tobjfld) : unit =
  let str_opt = function FldOpt -> "?" | _ -> "" in
  Fmt.fprintf fmt "%a%s: %a" Id.pp fn (str_opt fs) pp ft

let pp_tannot (fmt : Fmt.t) (t : t option) =
  Fmt.(fprintf fmt "%a" (pp_opt (fun fmt t -> fprintf fmt ": %a" pp t)) t)

let str (t : t) : string = Fmt.asprintf "%a" pp t

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
