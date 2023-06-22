open Core
open Encoding
open Expression
open Types

let expr_of_value (e : Value.t) : Expr.t =
  match e with
  | Value.Int x -> Expr.Val (Val.Int x)
  | Value.Str x -> Expr.Val (Val.Str x)
  | Value.Real x -> Expr.Val (Val.Flt x)
  | _ -> assert false

let translate_val (v : Val.t) : Expression.t =
  match v with
  | Val.Int x -> Integer.mk_val x
  | Val.Flt x -> Real.mk_val x
  | Val.Str x -> Strings.mk_val x
  | Val.Bool x -> Boolean.mk_val x
  | _ -> failwith ("translate_val: unsupported value '" ^ Val.str v ^ "'")

let translate_symbol (t : Type.t) : String.t -> Expression.t =
  match t with
  | Type.IntType -> mk_symbol_s `IntType
  | Type.FltType -> mk_symbol_s `RealType
  | Type.StrType -> mk_symbol_s `StrType
  | Type.BoolType -> mk_symbol_s `BoolType
  | _ ->
      failwith ("translate_symbol: unsupported symbol type '" ^ Type.str t ^ "'")

let translate_unop (t : Type.t option) (op : Operators.uopt) (e : Expression.t)
    : Expression.t =
  let open Type in
  let open Operators in
  let int_unop op e =
    let op' =
      match op with
      | Neg -> Integer.mk_neg
      | IntToFloat -> Real.mk_of_integer
      | _ ->
          Printf.printf "op: %s\n" (Operators.str_of_unopt op);
          assert false
    in
    op' e
  in
  let flt_unop op e =
    let op' =
      match op with
      | Neg -> Real.mk_neg
      | Abs -> Real.mk_abs
      | Sqrt -> Real.mk_sqrt
      | ToUint32 -> Real.mk_to_uint32
      | IsNaN -> fun _ -> Boolean.mk_val false
      | FloatToString -> Real.mk_to_string
      | FloatOfString -> Real.mk_of_string
      | _ ->
          Printf.printf "op: %s\n" (Operators.str_of_unopt op);
          assert false
    in
    op' e
  in
  let str_unop op e =
    let op' =
      match op with
      | StringLen | StringLenU -> Strings.mk_len
      | Trim -> Strings.mk_trim
      | FloatOfString -> Real.mk_of_string
      | _ ->
          Printf.printf "op: %s\n" (Operators.str_of_unopt op);
          assert false
    in
    op' e
  in

  let bool_unop op e =
    let op' =
      match op with
      | Not -> Boolean.mk_not
      | _ ->
          Printf.printf "op: %s\n" (Operators.str_of_unopt op);
          assert false
    in
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
      | _ ->
          Printf.printf "op: %s\n" (Operators.str_of_binopt_single op);
          assert false
    in
    op' e1 e2
  in
  let flt_binop op e1 e2 =
    let op' =
      match op with
      | Modulo -> assert false
      | Eq -> Real.mk_eq
      | Gt -> Real.mk_gt
      | Ge -> Real.mk_ge
      | Lt -> Real.mk_lt
      | Le -> Real.mk_le
      | Plus -> Real.mk_add
      | Minus -> Real.mk_sub
      | Times -> Real.mk_mul
      | Div -> Real.mk_div
      | Min -> Real.mk_min
      | Max -> Real.mk_max
      (* TODO: rewrite using `Real` constructors -- fails se we don't introduce
               encoding errors *)
      | BitwiseAnd -> assert false
      | BitwiseOr -> assert false
      | BitwiseXor -> assert false
      | ShiftLeft -> assert false
      | ShiftRight -> assert false
      | ShiftRightLogical -> assert false
      | _ ->
          Printf.printf "op: %s\n" (Operators.str_of_binopt_single op);
          assert false
    in
    op' e1 e2
  in
  let str_binop op e1 e2 =
    let op' =
      match op with
      | Snth -> Strings.mk_nth
      | Snth_u -> Strings.mk_nth
      | Eq -> Strings.mk_eq
      | _ ->
          Printf.printf "op: %s\n" (Operators.str_of_binopt_single op);
          assert false
    in
    op' e1 e2
  in
  let bool_binop op e1 e2 =
    let op' =
      match op with
      | Eq -> Boolean.mk_eq
      | Log_And -> Boolean.mk_and
      | Log_Or -> Boolean.mk_or
      | _ ->
          Printf.printf "op: %s\n" (Operators.str_of_binopt_single op);
          assert false
    in
    op' e1 e2
  in
  match (t1, t2) with
  | Some IntType, Some IntType -> int_binop op e1 e2
  | Some FltType, Some FltType -> flt_binop op e1 e2
  | Some StrType, _ -> str_binop op e1 e2
  | Some BoolType, Some BoolType -> bool_binop op e1 e2
  | None, _ | _, None -> failwith "translate_binop: untyped operator!"
  | _ ->
      failwith
        ("translate_binop: ill-typed or unsupported operator!op:"
        ^ Operators.str_of_binopt_single op
        ^ "e1: " ^ Expression.to_string e1 ^ "e2: " ^ Expression.to_string e2)

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
  let bool_triop op e1 e2 e3 =
    match op with
    | ITE -> Boolean.mk_ite e1 e2 e3
    | _ -> assert false
  in
  match (t1, t2, t3) with
  | Some BoolType, _, _ -> bool_triop op e1 e2 e3
  | Some StrType, _, _ -> str_triop op e1 e2 e3
  | None, _, _ | _, None, _ | _, _, None ->
      failwith
        ("translate_triop: untyped operator! "
        ^ Operators.str_of_triopt op "e1" "e2" "e3")
  | _ -> failwith "translate_triop: ill-typed or unsupported operator!"

let rec translate ?(b = false) (e : Expr.t) : Expression.t =
  if b then Printf.printf "\n\ntranslating: %s\n\n" (Expr.str e);
  match e with
  | Expr.Val v -> translate_val v
  | Expr.Symbolic (t, Expr.Val (Val.Str x)) -> translate_symbol t x
  | Expr.UnOpt (Operators.Sconcat, e) -> (
      let binop' e1 e2 = Binop (Str S.Concat, e1, e2) in
      match e with
      | Expr.NOpt (_, h :: t) ->
          List.fold_left ~init:(translate ~b:false h) ~f:binop'
            (List.map ~f:(translate ~b:false) t)
      | _ -> assert false)
  | Expr.UnOpt (op, e') ->
      let ty = Sval_typing.type_of e' in
      let e' = translate ~b:false e' in
      translate_unop ty op e'
  | Expr.BinOpt (op, e1, e2) ->
      let ty1 = Sval_typing.type_of e1 in
      let ty2 = Sval_typing.type_of e2 in
      let e1' = translate ~b:false e1 and e2' = translate ~b:false e2 in
      translate_binop ty1 ty2 op e1' e2'
  | Expr.TriOpt (op, e1, e2, e3) ->
      let ty1 = Sval_typing.type_of e1 in
      let ty2 = Sval_typing.type_of e2 in
      let ty3 = Sval_typing.type_of e3 in
      let e1' = translate ~b:false e1
      and e2' = translate ~b:false e2
      and e3' = translate ~b:false e3 in
      translate_triop ty1 ty2 ty3 op e1' e2' e3'
  | _ -> failwith (Expr.str e ^ ": Not translated!")
