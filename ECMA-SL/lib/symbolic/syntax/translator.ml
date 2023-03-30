open Encoding
open Expression
open Types

exception TODO

let expr_of_value (e : Expression.value) : Expr.t =
  match e with
  | Int i -> Expr.Val (Val.Int i)
  | Str s -> Expr.Val (Val.Str s)
  | Num (Types.F64 f) -> Expr.Val (Val.Flt (Int64.float_of_bits f))
  | _ -> assert false

let translate_binop (t : Type.t option) (op : Operators.bopt)
    (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  let open Type in
  let open Operators in
  match (t, op) with
  | Some IntType, Gt -> Relop (Int I.Gt, e1, e2)
  | Some IntType, Times -> Binop (Int I.Mul, e1, e2)
  | None, _ -> assert false
  | _, _ ->
      print_endline (Type.str (Option.get t));
      raise TODO

let rec translate (e : Expr.t) : Expression.t =
  try
    match e with
    | Expr.Val (Val.Int i) -> Val (Int i)
    | Expr.Symbolic (Type.IntType, Expr.Val (Val.Str x)) ->
        Symbolic (`IntType, x)
    | Expr.UnOpt (Operators.Not, _) -> failwith "Unop not"
    | Expr.BinOpt (op, e1, e2) ->
        let ty = Sval_typing.type_of e in
        let e1' = translate e1 and e2' = translate e2 in
        translate_binop ty op e1' e2'
    | _ -> failwith (Expr.str e ^ ": Not translated!")
  with TODO -> failwith (Expr.str e ^ ": Not translated!")
