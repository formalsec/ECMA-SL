let encode_neg (v : Sval.t) (ctx : Z3.context) : Z3.Expr.expr -> Z3.Expr.expr =
  match Sval_typing.type_of v with
  | Some t -> (
      match t with
      | Type.IntType -> Z3.Arithmetic.mk_unary_minus ctx
      | Type.FltType -> Z3.FloatingPoint.mk_neg ctx
      | _ -> failwith "Encoding: neg argument type unexpected.")
  | None -> failwith "Encoding: Neg argument has no type."
