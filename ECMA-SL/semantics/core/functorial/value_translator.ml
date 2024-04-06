open EslSyntax
open Encoding
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
  | Val.Int x -> Val (Value.Int x) @: Ty_int
  | Val.Flt x -> Val (Value.Real x) @: Ty_real
  | Val.Str x -> Val (Value.Str x) @: Ty_str
  | Val.Bool x -> Val (if x then Value.True else Value.False) @: Ty_bool
  | Val.Loc x -> Val (Value.Str (Loc.str x)) @: Ty_str
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
    | Neg -> Unop (Neg, e) @: Ty_int
    | IntToFloat -> Cvtop (Reinterpret_int, e) @: Ty_real
    | _ ->
      Log.out "op: %s\n" (Operator.str_of_unopt_single op);
      assert false
  in
  let flt_unop (op : Operator.unopt) e =
    match op with
    | Neg -> Unop (Neg, e) @: Ty_real
    | Abs -> Unop (Neg, e) @: Ty_real
    | Sqrt -> Unop (Neg, e) @: Ty_real
    (* | ToUint32 ->
      (* Real.mk_to_uint32 *)
      assert false *)
    (* | IsNaN -> Val Value.False @: Ty_bool *)
    | FloatToString -> Cvtop (ToString, e) @: Ty_real
    | StringToFloat -> Cvtop (OfString, e) @: Ty_real
    | Ceil -> Unop (Ceil, e) @: Ty_real
    | Floor -> Unop (Floor, e) @: Ty_real
    | FloatToInt (* | ToInt *) -> Cvtop (Reinterpret_float, e) @: Ty_int
    | _ ->
      Log.err "op: %s\n" (Operator.str_of_unopt_single op);
      assert false
  in
  let str_unop (op : Operator.unopt) e =
    match op with
    | StringLen (* | StringLenU *) -> Unop (Len, e) @: Ty_str
    (* | Trim -> Unop (Trim, e) @: Ty_str *)
    | StringToFloat -> Cvtop (OfString, e) @: Ty_real
    | ToCharCode (* | ToCharCodeU *) -> Cvtop (String_to_code, e) @: Ty_str
    | _ ->
      Log.out "op: %s, e: %a@." (Operator.str_of_unopt_single op) Expr.pp e;
      assert false
  in

  let bool_unop (op : Operator.unopt) e =
    match op with
    | LogicalNot -> Unop (Not, e) @: Ty_bool
    | _ ->
      Log.out "op: %s\n" (Operator.str_of_unopt_single op);
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
    | Eq -> Relop (Eq, e1, e2) @: Ty_int
    | Gt -> Relop (Gt, e1, e2) @: Ty_int
    | Ge -> Relop (Ge, e1, e2) @: Ty_int
    | Lt -> Relop (Lt, e1, e2) @: Ty_int
    | Le -> Relop (Le, e1, e2) @: Ty_int
    | Plus -> Binop (Add, e1, e2) @: Ty_int
    | Minus -> Binop (Sub, e1, e2) @: Ty_int
    | Times -> Binop (Mul, e1, e2) @: Ty_int
    | Div -> Binop (Div, e1, e2) @: Ty_int
    | _ ->
      Log.out "op: %s\n" (Operator.str_of_binopt_single op);
      assert false
  in
  let flt_binop op e1 e2 =
    match op with
    | Modulo -> assert false
    | Eq -> Relop (Eq, e1, e2) @: Ty_real
    | Gt -> Relop (Gt, e1, e2) @: Ty_real
    | Ge -> Relop (Ge, e1, e2) @: Ty_real
    | Lt -> Relop (Lt, e1, e2) @: Ty_real
    | Le -> Relop (Le, e1, e2) @: Ty_real
    | Plus -> Binop (Add, e1, e2) @: Ty_real
    | Minus -> Binop (Sub, e1, e2) @: Ty_real
    | Times -> Binop (Mul, e1, e2) @: Ty_real
    | Div -> Binop (Div, e1, e2) @: Ty_real
    (* | Min -> Binop (Min, e1, e2) @: Ty_real *)
    (* | Max -> Binop (Max, e1, e2) @: Ty_real *)
    (* TODO: rewrite using `Real` constructors -- fails se we don't introduce
             encoding errors *)
    | BitwiseAnd -> assert false
    | BitwiseOr -> assert false
    | BitwiseXor -> assert false
    | ShiftLeft -> assert false
    | ShiftRight -> assert false
    | ShiftRightLogical -> assert false
    | _ ->
      Log.out "op: %s\n" (Operator.str_of_binopt_single op);
      assert false
  in
  let str_binop op e1 e2 =
    match op with
    | StringNth (* | StringNthU *) -> Binop (Nth, e1, e2) @: Ty_str
    | Eq -> Relop (Eq, e1, e2) @: Ty_str
    | _ ->
      Log.out "op: %s\n" (Operator.str_of_binopt_single op);
      assert false
  in
  let bool_binop (op : Operator.binopt) e1 e2 =
    match op with
    | Eq -> Relop (Eq, e1, e2) @: Ty_bool
    | LogicalAnd -> Binop (And, e1, e2) @: Ty_bool
    | LogicalOr -> Binop (Or, e1, e2) @: Ty_bool
    | _ ->
      Log.out "op: %s\n" (Operator.str_of_binopt_single op);
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
    (* | StringSubstrU *) | StringSubstr -> Triop (Substr, e1, e2, e3) @: Ty_str
    | _ -> assert false
  in
  let bool_triop (op : Operator.triopt) e1 e2 e3 =
    match op with
    | ITE -> Triop (Ite, e1, e2, e3) @: Ty_bool
    | _ -> assert false
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
  if b then Log.out "\n\ntranslating: %a\n\n" pp v;
  match v with
  | Val v -> translate_val v
  | Symbolic (t, Val (Val.Str x)) -> translate_symbol t x
  | UnOpt (Operator.StringConcat, e) -> (
    let binop' e1 e2 = Binop (Concat, e1, e2) @: Ty_str in
    match e with
    | NOpt (_, h :: t) ->
      List.fold_left binop' (translate ~b:false h)
        (List.map (translate ~b:false) t)
    | _ -> assert false )
  | UnOpt (op, e') ->
    let ty = Value_typing.type_of e' in
    let e' = translate ~b:false e' in
    translate_unop ty op e'
  | BinOpt (op, e1, e2) ->
    let ty1 = Value_typing.type_of e1 in
    let ty2 = Value_typing.type_of e2 in
    let e1' = translate ~b:false e1
    and e2' = translate ~b:false e2 in
    translate_binop ty1 ty2 op e1' e2'
  | TriOpt (op, e1, e2, e3) ->
    let ty1 = Value_typing.type_of e1 in
    let ty2 = Value_typing.type_of e2 in
    let ty3 = Value_typing.type_of e3 in
    let e1' = translate ~b:false e1
    and e2' = translate ~b:false e2
    and e3' = translate ~b:false e3 in
    translate_triop ty1 ty2 ty3 op e1' e2' e3'
  | _ -> Log.fail "%a: Not translated!" pp v
