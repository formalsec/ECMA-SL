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

let translate_val (v : Val.t) : Expression.t =
  match v with
  | Val.Int x -> Integer.mk_val x
  | Val.Flt x -> FloatingPoint.mk_val x `F32Type
  | Val.Str x -> Strings.mk_val x
  | Val.Bool x -> Boolean.mk_val x
  | _ -> failwith ("translate_val: unsupported value '" ^ Val.str v ^ "'")

let translate_symbol (t : Type.t) : String.t -> Expression.t =
  match t with
  | Type.IntType -> mk_symbol `IntType
  | Type.FltType -> mk_symbol `F32Type
  | Type.StrType -> mk_symbol `StrType
  | Type.BoolType -> mk_symbol `BoolType
  | _ ->
      failwith ("translate_symbol: unsupported symbol type '" ^ Type.str t ^ "'")

let translate_unop (t : Type.t option) (op : Operators.uopt) (e : Expression.t)
    : Expression.t =
  let open Type in
  let open Operators in
  let int_unop op e =
    let op' = match op with Neg -> Integer.mk_neg | _ -> assert false in
    op' e
  in
  let flt_unop op e =
    let op' =
      match op with
      | Neg -> FloatingPoint.mk_neg
      | Abs -> FloatingPoint.mk_abs
      | Sqrt -> FloatingPoint.mk_sqrt
      | IsNaN -> FloatingPoint.mk_is_nan
      (* TODO: rewrite using `FloatingPoint` constructors *)
      | BitwiseNot ->
          fun e _ ->
            Cvtop
              ( F32 F32.ConvertSI32,
                Unop (I32 I32.Not, Cvtop (I32 I32.TruncSF32, e)) )
      | _ -> assert false
    in
    op' e `F32Type
  in
  let str_unop op e =
    let op' = match op with 
    | StringLen | StringLenU-> Strings.mk_len 
    | _ -> assert false in
    op' e
  in

  let bool_unop op e =
    let op' = match op with Not -> Boolean.mk_not | _ -> assert false in
    op' e
  in
  (* dispatch *)
  match t with
  | Some IntType -> int_unop op e
  | Some FltType -> flt_unop op e
  | Some StrType -> str_unop op e
  | Some BoolType -> bool_unop op e
  | Some t -> failwith "translate_unop: ill-typed or unsupported operator!"
  | None -> failwith "translate_unop: untyped operator!"

let translate_binop (t1 : Type.t option) (t2 : Type.t option)
    (op : Operators.bopt) (e1 : Expression.t) (e2 : Expression.t) : Expression.t
    =
  let open Type in
  let open Operators in
  let int_binop op e1 e2 =
    let op' =
      match op with
      | Eq -> Integer.mk_eq
      | Gt -> Integer.mk_gt
      | Ge -> Integer.mk_ge
      | Lt -> Integer.mk_lt
      | Le -> Integer.mk_le
      | Plus -> Integer.mk_add
      | Minus -> Integer.mk_add
      | Times -> Integer.mk_mul
      | Div -> Integer.mk_div
      | _ -> assert false
    in
    op' e1 e2
  in
  let flt_binop op e1 e2 =
    let op' =
      match op with
      | Modulo -> FloatingPoint.mk_rem
      | Eq -> FloatingPoint.mk_eq
      | Gt -> FloatingPoint.mk_gt
      | Ge -> FloatingPoint.mk_ge
      | Lt -> FloatingPoint.mk_lt
      | Le -> FloatingPoint.mk_le
      | Plus -> FloatingPoint.mk_add
      | Minus -> FloatingPoint.mk_sub
      | Times -> FloatingPoint.mk_mul
      | Div -> FloatingPoint.mk_div
      | Min -> FloatingPoint.mk_min
      | Max -> FloatingPoint.mk_max
      (* TODO: rewrite using `FloatingPoint` constructors *)
      | BitwiseAnd ->
          fun e1 e2 _ ->
            Cvtop
              ( F32 F32.ConvertSI32,
                Binop
                  ( I32 I32.And,
                    Cvtop (I32 I32.TruncSF32, e1),
                    Cvtop (I32 I32.TruncSF32, e2) ) )
      | BitwiseOr ->
          fun e1 e2 _ ->
            Cvtop
              ( F32 F32.ConvertSI32,
                Binop
                  ( I32 I32.Or,
                    Cvtop (I32 I32.TruncSF32, e1),
                    Cvtop (I32 I32.TruncSF32, e2) ) )
      | BitwiseXor ->
          fun e1 e2 _ ->
            Cvtop
              ( F32 F32.ConvertSI32,
                Binop
                  ( I32 I32.Xor,
                    Cvtop (I32 I32.TruncSF32, e1),
                    Cvtop (I32 I32.TruncSF32, e2) ) )
      | ShiftLeft ->
          fun e1 e2 _ ->
            Cvtop
              ( F32 F32.ConvertSI32,
                Binop
                  ( I32 I32.Shl,
                    Cvtop (I32 I32.TruncSF32, e1),
                    Cvtop (I32 I32.TruncSF32, e2) ) )
      | ShiftRight ->
          fun e1 e2 _ ->
            Cvtop
              ( F32 F32.ConvertSI32,
                Binop
                  ( I32 I32.ShrS,
                    Cvtop (I32 I32.TruncSF32, e1),
                    Cvtop (I32 I32.TruncSF32, e2) ) )
      | ShiftRightLogical ->
          fun e1 e2 _ ->
            Cvtop
              ( F32 F32.ConvertSI32,
                Binop
                  ( I32 I32.ShrU,
                    Cvtop (I32 I32.TruncSF32, e1),
                    Cvtop (I32 I32.TruncSF32, e2) ) )
      | _ -> assert false
    in
    op' e1 e2 `F32Type
  in
  let str_binop op e1 e2 =
    let op' =
      match op with
      | Snth -> Strings.mk_nth
      | Eq -> Strings.mk_eq
      | _ -> assert false
    in
    op' e1 e2
  in
  let bool_binop op e1 e2 =
    let op' =
      match op with
      | Eq -> Boolean.mk_eq
      | Log_And -> Boolean.mk_and
      | Log_Or -> Boolean.mk_or
      | _ -> assert false
    in
    op' e1 e2
  in
  match (t1, t2) with
  | Some IntType, Some IntType -> int_binop op e1 e2
  | Some FltType, Some FltType -> flt_binop op e1 e2
  | Some StrType, _ -> str_binop op e1 e2
  | Some BoolType, Some BoolType -> bool_binop op e1 e2
  | None, _ | _, None -> failwith "translate_binop: untyped operator!"
  | _ -> failwith "translate_binop: ill-typed or unsupported operator!"

let translate_triop (t1 : Type.t option) (t2 : Type.t option)
    (t3 : Type.t option) (op : Operators.topt) (e1 : Expression.t)
    (e2 : Expression.t) (e3 : Expression.t) =
  let open Type in
  let open Operators in
  let str_triop op e1 e2 e3 =
    match op with
    | Ssubstr -> Strings.mk_substr e1 ~pos:e2 ~len:e3
    | _ -> assert false
  in
  match (t1, t2, t3) with
  | Some StrType, _, _ -> str_triop op e1 e2 e3
  | None, _, _ | _, None, _ | _, _, None ->
      failwith ("translate_triop: untyped operator! " ^ (Operators.str_of_triopt op "e1" "e2" "e3"))
  | _ -> failwith "translate_triop: ill-typed or unsupported operator!"

let rec translate (e : Expr.t) : Expression.t =
  match e with
  | Expr.Val v -> translate_val v
  | Expr.Symbolic (t, Expr.Val (Val.Str x)) -> translate_symbol t x
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
