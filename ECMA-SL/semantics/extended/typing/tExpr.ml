open EslBase
open EslSyntax
open EslSyntax.Source

let rec type_expr (e : EExpr.t) : EType.t =
  match e.it with
  | Val v -> type_val v @> e.at
  | Var _ -> EType.AnyType @> e.at (* TODO *)
  | GVar _ -> EType.AnyType @> e.at (* TODO *)
  | Const _ -> EType.AnyType @> e.at (* TODO *)
  | UnOpt _ -> EType.AnyType @> e.at (* TODO *)
  | BinOpt _ -> EType.AnyType @> e.at (* TODO *)
  | TriOpt _ -> EType.AnyType @> e.at (* TODO *)
  | NOpt _ -> EType.AnyType @> e.at (* TODO *)
  | Call _ -> EType.AnyType @> e.at (* TODO *)
  | ECall _ -> EType.AnyType @> e.at (* TODO *)
  | NewObj _ -> EType.AnyType @> e.at (* TODO *)
  | Lookup _ -> EType.AnyType @> e.at (* TODO *)
  | Curry _ -> EType.AnyType @> e.at (* TODO *)
  | Symbolic _ -> EType.AnyType @> e.at (* TODO *)

and type_val (v : Val.t) : EType.t' =
  let err v = Internal_error.UnexpectedEval (Some (v ^ " val")) in
  match v with
  | Val.Null -> NullType
  | Val.Void -> Internal_error.(throw __FUNCTION__ (err "void"))
  | Val.Int i -> LiteralType (IntegerLit i)
  | Val.Flt f -> LiteralType (FloatLit f)
  | Val.Str s -> LiteralType (StringLit s)
  | Val.Bool b -> LiteralType (BooleanLit b)
  | Val.Symbol s -> LiteralType (SymbolLit s)
  | Val.Loc _ -> Internal_error.(throw __FUNCTION__ (err "loc"))
  | Val.Arr _ -> Internal_error.(throw __FUNCTION__ (err "array"))
  | Val.List _ -> Internal_error.(throw __FUNCTION__ (err "list"))
  | Val.Tuple _ -> Internal_error.(throw __FUNCTION__ (err "tuple"))
  | Val.Byte _ -> Internal_error.(throw __FUNCTION__ (err "byte"))
  | Val.Type _ -> AnyType (* TODO *)
  | Val.Curry _ -> AnyType (* TODO *)
