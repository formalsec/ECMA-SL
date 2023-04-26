open Core
open Encoding
open Expression
open Types

let expr_of_value (e : Expression.value) : Expr.t =
  match e with
  | Int i -> Expr.Val (Val.Int i)
  | Str s -> Expr.Val (Val.Str s)
  | Num (Types.F32 f) -> Expr.Val (Val.Flt (Int32.float_of_bits f))
  | _ -> assert false

let translate_binop (t1 : Type.t option) (t2 : Type.t option)
    (op : Operators.bopt) (e1 : Expression.t) (e2 : Expression.t) : Expression.t
    =
  let open Type in
  let open Operators in
  match (t1, t2, op) with
  | Some IntType, Some IntType, Eq -> Relop (Int I.Eq, e1, e2)
  | Some IntType, Some IntType, Gt -> Relop (Int I.Gt, e1, e2)
  | Some IntType, Some IntType, Ge -> Relop (Int I.Ge, e1, e2)
  | Some IntType, Some IntType, Lt -> Relop (Int I.Lt, e1, e2)
  | Some IntType, Some IntType, Le -> Relop (Int I.Le, e1, e2)
  | Some IntType, Some IntType, Plus -> Binop (Int I.Add, e1, e2)
  | Some IntType, Some IntType, Minus -> Binop (Int I.Sub, e1, e2)
  | Some IntType, Some IntType, Times -> Binop (Int I.Mul, e1, e2)
  | Some IntType, Some IntType, Div -> Binop (Int I.Div, e1, e2)
  | Some FltType, Some FltType, BitwiseAnd ->
      Cvtop
        ( F32 F32.ConvertSI32,
          Binop
            ( I32 I32.And,
              Cvtop (I32 I32.TruncSF32, e1),
              Cvtop (I32 I32.TruncSF32, e2) ) )
  | Some FltType, Some FltType, BitwiseOr ->
      Cvtop
        ( F32 F32.ConvertSI32,
          Binop
            ( I32 I32.Or,
              Cvtop (I32 I32.TruncSF32, e1),
              Cvtop (I32 I32.TruncSF32, e2) ) )
  | Some FltType, Some FltType, BitwiseXor ->
      Cvtop
        ( F32 F32.ConvertSI32,
          Binop
            ( I32 I32.Xor,
              Cvtop (I32 I32.TruncSF32, e1),
              Cvtop (I32 I32.TruncSF32, e2) ) )
  | Some FltType, Some FltType, ShiftLeft ->
      Cvtop
        ( F32 F32.ConvertSI32,
          Binop
            ( I32 I32.Shl,
              Cvtop (I32 I32.TruncSF32, e1),
              Cvtop (I32 I32.TruncSF32, e2) ) )
  | Some FltType, Some FltType, ShiftRight ->
      Cvtop
        ( F32 F32.ConvertSI32,
          Binop
            ( I32 I32.ShrS,
              Cvtop (I32 I32.TruncSF32, e1),
              Cvtop (I32 I32.TruncSF32, e2) ) )
  | Some FltType, Some FltType, ShiftRightLogical ->
      Cvtop
        ( F32 F32.ConvertSI32,
          Binop
            ( I32 I32.ShrU,
              Cvtop (I32 I32.TruncSF32, e1),
              Cvtop (I32 I32.TruncSF32, e2) ) )
  | Some FltType, Some FltType, Modulo -> Binop (F32 F32.Rem, e1, e2)
  | Some FltType, Some FltType, Eq -> Relop (F32 F32.Eq, e1, e2)
  | Some FltType, Some FltType, Gt -> Relop (F32 F32.Gt, e1, e2)
  | Some FltType, Some FltType, Ge -> Relop (F32 F32.Ge, e1, e2)
  | Some FltType, Some FltType, Lt -> Relop (F32 F32.Lt, e1, e2)
  | Some FltType, Some FltType, Le -> Relop (F32 F32.Le, e1, e2)
  | Some FltType, Some FltType, Plus -> Binop (F32 F32.Add, e1, e2)
  | Some FltType, Some FltType, Minus -> Binop (F32 F32.Sub, e1, e2)
  | Some FltType, Some FltType, Times -> Binop (F32 F32.Mul, e1, e2)
  | Some FltType, Some FltType, Div -> Binop (F32 F32.Div, e1, e2)
  | Some FltType, Some FltType, Min -> Binop (F32 F32.Min, e1, e2)
  | Some FltType, Some FltType, Max -> Binop (F32 F32.Max, e1, e2)
  | Some BoolType, Some BoolType, Eq -> Relop (Bool B.Eq, e1, e2)
  | Some BoolType, Some BoolType, Log_And -> Binop (Bool B.And, e1, e2)
  | Some BoolType, Some BoolType, Log_Or -> Binop (Bool B.Or, e1, e2)
  | Some StrType, Some IntType, Snth -> Binop (Str S.Nth, e1, e2)
  | Some StrType, Some StrType, Eq -> Relop (Str S.Eq, e1, e2)
  | None, _, op -> assert false
  | _, _, _ -> failwith "TODO: binop"

let translate_triop (t1 : Type.t option) (t2 : Type.t option)
    (t3 : Type.t option) (op : Operators.topt) (e1 : Expression.t)
    (e2 : Expression.t) (e3 : Expression.t) =
  let open Type in
  match (t1, t2, t3, op) with
  | Some StrType, Some IntType, Some IntType, s_substr ->
      Triop (Str S.SubStr, e1, e2, e3)
  | _, _, _, _ -> failwith "TODO: triop"

let translate_unop (t : Type.t option) (op : Operators.uopt) (e : Expression.t)
    : Expression.t =
  let open Type in
  let open Operators in
  match (t, op) with
  | Some FltType, Neg -> Unop (F32 F32.Neg, e)
  | Some FltType, Abs -> Unop (F32 F32.Abs, e)
  | Some FltType, Sqrt -> Unop (F32 F32.Sqrt, e)
  | Some FltType, BitwiseNot ->
      Cvtop
        (F32 F32.ConvertSI32, Unop (I32 I32.Not, Cvtop (I32 I32.TruncSF32, e)))
  | Some FltType, IsNaN -> Unop (F32 F32.IsNan, e)
  | Some IntType, Neg -> Unop (Int I.Neg, e)
  | Some StrType, StringLen -> Unop (Str S.Len, e)
  | Some BoolType, Not -> Unop (Bool B.Not, e)
  | Some _, _ -> failwith "TODO: unop"
  | None, op -> failwith ("Type not possible for" ^ Expression.to_string e)

let rec translate (e : Expr.t) : Expression.t =
  match e with
  | Expr.Val (Val.Int i) -> Val (Int i)
  | Expr.Val (Val.Str s) -> Val (Str s)
  | Expr.Val (Val.Bool b) -> Val (Bool b)
  | Expr.Val (Val.Flt f) -> Val (Num (F32 (Int32.bits_of_float f)))
  | Expr.Symbolic (Type.IntType, Expr.Val (Val.Str x)) -> Symbolic (`IntType, x)
  | Expr.Symbolic (Type.StrType, Expr.Val (Val.Str x)) -> Symbolic (`StrType, x)
  | Expr.Symbolic (Type.BoolType, Expr.Val (Val.Str x)) ->
      Symbolic (`BoolType, x)
  | Expr.Symbolic (Type.FltType, Expr.Val (Val.Str x)) -> Symbolic (`F32Type, x)
  | Expr.UnOpt (Operators.Sconcat, e) -> (
      let binop' e1 e2 = Binop (Str S.Concat, e1, e2) in
      match e with
      | Expr.NOpt (_, h :: t) ->
          List.fold_left ~init:(translate h) ~f:binop' (List.map ~f:translate t)
      | _ -> assert false)
  | Expr.UnOpt (op, e') ->
      let ty = Sval_typing.type_of e' in
      let e' = translate e' in
      translate_unop ty op e'
  | Expr.BinOpt (op, e1, e2) ->
      let ty1 = Sval_typing.type_of e1 in
      let ty2 = Sval_typing.type_of e2 in
      let e1' = translate e1 and e2' = translate e2 in
      translate_binop ty1 ty2 op e1' e2'
  | Expr.TriOpt (op, e1, e2, e3) ->
      let ty1 = Sval_typing.type_of e1 in
      let ty2 = Sval_typing.type_of e2 in
      let ty3 = Sval_typing.type_of e3 in
      let e1' = translate e1 and e2' = translate e2 and e3' = translate e3 in
      translate_triop ty1 ty2 ty3 op e1' e2' e3'
  | _ -> failwith (Expr.str e ^ ": Not translated!")
