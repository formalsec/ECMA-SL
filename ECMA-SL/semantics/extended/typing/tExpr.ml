open EslBase
open EslSyntax
open EslSyntax.Source

let rec type_expr (e : EExpr.t) : EType.t = type_expr' e @> e.at

and type_expr' (e : EExpr.t) : EType.t' =
  let texprs es = List.map type_expr es in
  match e.it with
  | Val v -> type_val v
  | Var _ -> EType.AnyType (* TODO *)
  | GVar _ -> EType.AnyType (* TODO *)
  | Const c -> TOperator.type_const c
  | UnOpt (op, e') -> texprs [ e' ] |> TOperator.type_unopt op
  | BinOpt (op, e1, e2) -> texprs [ e1; e2 ] |> TOperator.type_binopt op
  | TriOpt (op, e1, e2, e3) -> texprs [ e1; e2; e3 ] |> TOperator.type_triopt op
  | NOpt (op, es) -> texprs es |> TOperator.type_nopt op
  | Call _ -> EType.AnyType (* TODO *)
  | ECall _ -> EType.AnyType (* TODO *)
  | NewObj _ -> EType.AnyType (* TODO *)
  | Lookup _ -> EType.AnyType (* TODO *)
  | Curry _ -> EType.AnyType (* TODO *)
  | Symbolic _ -> EType.AnyType (* TODO *)

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
