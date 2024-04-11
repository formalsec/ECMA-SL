open EslSyntax
open Smtml
open EslBase
open Expr
open Ty
open Symbolic_value.M

let expr_of_value : Expr.expr -> value = function
  | Val (Value.Int x) -> Val (Val.Int x)
  | Val (Value.Str x) -> Val (Val.Str x)
  | Val (Value.Real x) -> Val (Val.Flt x)
  | _ -> assert false

let translate_val (v : Val.t) : Expr.t =
  match v with
  | Val.Int x -> value (Value.Int x)
  | Val.Flt x -> value (Value.Real x)
  | Val.Str x -> value (Value.Str x)
  | Val.Bool x -> value (if x then Value.True else Value.False)
  | Val.Loc x -> value (Value.Str (Loc.str x))
  | _ -> failwith ("translate_val: unsupported value '" ^ Val.str v ^ "'")

let translate_symbol (t : Type.t) (x : string) : Expr.t =
  match t with
  | Type.IntType -> Expr.mk_symbol Symbol.(x @: Ty_int)
  | Type.FltType -> Expr.mk_symbol Symbol.(x @: Ty_real)
  | Type.StrType -> Expr.mk_symbol Symbol.(x @: Ty_str)
  | Type.BoolType -> Expr.mk_symbol Symbol.(x @: Ty_bool)
  | _ ->
    failwith ("translate_symbol: unsupported symbol type '" ^ Type.str t ^ "'")

let translate_unop (t : Type.t option) (op : Operator.unopt) (e : Expr.t) :
  Expr.t =
  let open Type in
  let open Operator in
  let int_unop (op : Operator.unopt) e =
    match op with
    | Neg -> unop Ty_int Neg e
    | IntToFloat -> cvtop Ty_int Reinterpret_int e
    | _ ->
      Log.stdout "op: %s\n" (Operator.str_of_unopt_single op);
      assert false
  in
  let flt_unop (op : Operator.unopt) e =
    match op with
    | Neg -> unop Ty_real Neg e
    | Abs -> unop Ty_real Abs e
    | Sqrt -> unop Ty_real Sqrt e
    | Ceil -> unop Ty_real Ceil e
    | Floor -> unop Ty_real Floor e
    | FloatToString -> cvtop Ty_real ToString e
    | StringToFloat -> cvtop Ty_str OfString e
    | FloatToInt -> cvtop Ty_int Reinterpret_float e
    | _ ->
      Log.stderr "op: %s\n" (Operator.str_of_unopt_single op);
      assert false
  in
  let str_unop (op : Operator.unopt) e =
    match op with
    | StringLen -> unop Ty_str Length e
    | StringToFloat -> cvtop Ty_real OfString e
    | ToCharCode -> cvtop Ty_str String_to_code e
    | _ ->
      Log.stdout "op: %s, e: %a@." (Operator.str_of_unopt_single op) Expr.pp e;
      assert false
  in

  let bool_unop (op : Operator.unopt) e =
    match op with
    | LogicalNot -> Expr.Bool.not e
    | _ ->
      Log.stdout "op: %s\n" (Operator.str_of_unopt_single op);
      assert false
  in
  (* dispatch *)
  match t with
  | Some IntType -> int_unop op e
  | Some FltType -> flt_unop op e
  | Some StrType -> str_unop op e
  | Some BoolType -> bool_unop op e
  | Some _ -> failwith "translate_unop: ill-typed or unsupported operator!"
  | None -> failwith "translate_unop: untyped operator!"

let translate_binop (t1 : Type.t option) (t2 : Type.t option)
  (op : Operator.binopt) (e1 : Expr.t) (e2 : Expr.t) : Expr.t =
  let open Type in
  let open Operator in
  let int_binop (op : Operator.binopt) e1 e2 =
    match op with
    | Eq -> relop Ty_bool Eq e1 e2
    | Gt -> relop Ty_int Gt e1 e2
    | Ge -> relop Ty_int Ge e1 e2
    | Lt -> relop Ty_int Lt e1 e2
    | Le -> relop Ty_int Le e1 e2
    | Plus -> binop Ty_int Add e1 e2
    | Minus -> binop Ty_int Sub e1 e2
    | Times -> binop Ty_int Mul e1 e2
    | Div -> binop Ty_int Div e1 e2
    | _ ->
      Log.stdout "op: %s\n" (Operator.str_of_binopt_single op);
      assert false
  in
  let flt_binop op e1 e2 =
    match op with
    | Modulo -> assert false
    | Eq -> relop Ty_bool Eq e1 e2
    | Gt -> relop Ty_real Gt e1 e2
    | Ge -> relop Ty_real Ge e1 e2
    | Lt -> relop Ty_real Lt e1 e2
    | Le -> relop Ty_real Le e1 e2
    | Plus -> binop Ty_real Add e1 e2
    | Minus -> binop Ty_real Sub e1 e2
    | Times -> binop Ty_real Mul e1 e2
    | Div -> binop Ty_real Div e1 e2
    | _ ->
      Log.stdout "op: %s\n" (Operator.str_of_binopt_single op);
      assert false
  in
  let str_binop op e1 e2 =
    match op with
    | StringNth -> binop Ty_str At e1 e2
    | Eq -> relop Ty_bool Eq e1 e2
    | _ ->
      Log.stdout "op: %s\n" (Operator.str_of_binopt_single op);
      assert false
  in
  let bool_binop (op : Operator.binopt) e1 e2 =
    match op with
    | Eq -> Expr.Bool.(e1 = e2)
    | LogicalAnd -> Expr.Bool.and_ e1 e2
    | LogicalOr -> Expr.Bool.or_ e1 e2
    | _ ->
      Log.stdout "op: %s\n" (Operator.str_of_binopt_single op);
      assert false
  in
  match (t1, t2) with
  | (Some IntType, Some IntType) -> int_binop op e1 e2
  | (Some FltType, Some FltType) -> flt_binop op e1 e2
  | (Some StrType, _) -> str_binop op e1 e2
  | (Some BoolType, Some BoolType) -> bool_binop op e1 e2
  | (None, _) | (_, None) -> failwith "translate_binop: untyped operator!"
  | _ ->
    failwith
      ( "translate_binop: ill-typed or unsupported operator!op:"
      ^ Operator.str_of_binopt_single op
      ^ "e1: "
      ^ Expr.to_string e1
      ^ "e2: "
      ^ Expr.to_string e2 )

let translate_triop (t1 : Type.t option) (t2 : Type.t option)
  (t3 : Type.t option) (op : Operator.triopt) (e1 : Expr.t) (e2 : Expr.t)
  (e3 : Expr.t) =
  let open Type in
  let open Operator in
  let str_triop (op : Operator.triopt) e1 e2 e3 =
    match op with
    (* | StringSubstrU *)
    | StringSubstr -> triop Ty_str String_extract e1 e2 e3
    | _ -> assert false
  in
  let bool_triop (op : Operator.triopt) e1 e2 e3 =
    match op with ITE -> triop Ty_bool Ite e1 e2 e3 | _ -> assert false
  in
  match (t1, t2, t3) with
  | (Some BoolType, _, _) -> bool_triop op e1 e2 e3
  | (Some StrType, _, _) -> str_triop op e1 e2 e3
  | (None, _, _) | (_, None, _) | (_, _, None) ->
    failwith
      ( "translate_triop: untyped operator! "
      ^ Operator.str_of_triopt Format.pp_print_string op "e1" "e2" "e3" )
  | _ -> failwith "translate_triop: ill-typed or unsupported operator!"

let rec translate ?(b = false) (v : value) : Expr.t =
  if b then Log.stdout "\n\ntranslating: %a\n\n" pp v;
  match v with
  | Val v -> translate_val v
  | Symbolic (t, Val (Val.Str x)) -> translate_symbol t x
  | UnOpt (Operator.StringConcat, e) -> (
    let binop' e1 e2 = naryop Ty_str Concat [e1; e2] in
    match e with
    | NOpt (_, h :: t) ->
      List.fold_left binop' (translate h) (List.map translate t)
    | _ -> assert false )
  | UnOpt (op, e') ->
    let ty = Value_typing.type_of e' in
    let e' = translate e' in
    translate_unop ty op e'
  | BinOpt (op, e1, e2) ->
    let ty1 = Value_typing.type_of e1 in
    let ty2 = Value_typing.type_of e2 in
    let e1' = translate e1
    and e2' = translate e2 in
    translate_binop ty1 ty2 op e1' e2'
  | TriOpt (op, e1, e2, e3) ->
    let ty1 = Value_typing.type_of e1 in
    let ty2 = Value_typing.type_of e2 in
    let ty3 = Value_typing.type_of e3 in
    let e1' = translate e1
    and e2' = translate e2
    and e3' = translate e3 in
    translate_triop ty1 ty2 ty3 op e1' e2' e3'
  | _ -> Log.fail "%a: Not translated!" pp v
