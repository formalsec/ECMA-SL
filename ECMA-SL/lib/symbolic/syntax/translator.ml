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
  | Some IntType, Eq -> Relop (Int I.Eq, e1, e2)
  | Some IntType, Gt -> Relop (Int I.Gt, e1, e2)
  | Some IntType, Ge -> Relop (Int I.Ge, e1, e2)
  | Some IntType, Lt -> Relop (Int I.Lt, e1, e2)
  | Some IntType, Le -> Relop (Int I.Le, e1, e2)
  | Some IntType, Plus -> Binop (Int I.Add, e1, e2)
  | Some IntType, Minus -> Binop (Int I.Sub, e1, e2)
  | Some IntType, Times -> Binop (Int I.Mul, e1, e2)
  | Some IntType, Div -> Binop (Int I.Div, e1, e2)
  | Some IntType, BitwiseAnd -> Binop (Int I.And, e1, e2)
  | Some IntType, BitwiseOr -> Binop (Int I.Or, e1, e2)
  | Some IntType, BitwiseXor -> Binop (Int I.Xor, e1, e2)
  | Some IntType, ShiftLeft -> Binop (Int I.Shl, e1, e2)
  | Some IntType, ShiftRight -> Binop (Int I.ShrA, e1, e2)
  | Some IntType, ShiftRightLogical -> Binop (Int I.ShrL, e1, e2)
  | Some IntType, Modulo -> Binop (Int I.Rem, e1, e2)
  | Some FltType, Eq -> Relop (F64 F64.Eq, e1, e2)
  | Some FltType, Gt -> Relop (F64 F64.Gt, e1, e2)
  | Some FltType, Ge -> Relop (F64 F64.Ge, e1, e2)
  | Some FltType, Lt -> Relop (F64 F64.Lt, e1, e2)
  | Some FltType, Le -> Relop (F64 F64.Le, e1, e2)
  | Some FltType, Plus -> Binop (F64 F64.Add, e1, e2)
  | Some FltType, Minus -> Binop (F64 F64.Sub, e1, e2)
  | Some FltType, Times -> Binop (F64 F64.Mul, e1, e2)
  | Some FltType, Div -> Binop (F64 F64.Div, e1, e2)
  | Some FltType, Min -> Binop (F64 F64.Min, e1, e2)
  | Some FltType, Max -> Binop (F64 F64.Max, e1, e2)
  | _, Log_And -> Binop (Bool B.And, e1, e2)
  | _, Log_Or -> Binop (Bool B.Or, e1, e2)
  | None, _ -> assert false
  | _, _ ->
      print_endline (Type.str (Option.get t));
      raise TODO

let translate_unop (t : Type.t option) (op : Operators.uopt) (e : Expression.t)
    : Expression.t =
  let open Type in
  let open Operators in
  match (t, op) with
  | Some FltType, Neg -> Unop (F64 F64.Neg, e)
  | Some FltType, Abs -> Unop (F64 F64.Abs, e)
  | Some FltType, Sqrt -> Unop (F64 F64.Sqrt, e)
  | Some IntType, Neg -> Unop (Int I.Neg, e)
  | Some _, Not -> Unop (Bool B.Not, e)
  | _, _ ->
      print_endline (Type.str (Option.get t));
      raise TODO

let rec translate (e : Expr.t) : Expression.t =
  try
    match e with
    | Expr.Val (Val.Int i) -> Val (Int i)
    | Expr.Symbolic (Type.IntType, Expr.Val (Val.Str x)) ->
        Symbolic (`IntType, x)
    | Expr.UnOpt (op, e) ->
        let ty = Sval_typing.type_of e in
        let e' = translate e in
        translate_unop ty op e'
    | Expr.BinOpt (op, e1, e2) ->
        let ty = Sval_typing.type_of e in
        let e1' = translate e1 and e2' = translate e2 in
        translate_binop ty op e1' e2'
    | _ -> failwith (Expr.str e ^ ": Not translated!")
  with TODO -> failwith (Expr.str e ^ ": Not translated!")
