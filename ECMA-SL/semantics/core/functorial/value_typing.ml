open EslBase
open EslSyntax
module Value = Symbolic_value.M
(* TODO: this is to remove when we mirgate to the encoding values *)

let type_of_neg = function
  | Type.IntType -> Some Type.IntType
  | Type.FltType -> Some Type.FltType
  | _ -> None

let type_of_bitwise_not = function
  | Type.IntType -> Some Type.IntType
  | _ -> None

let type_of_not = function Type.BoolType -> Some Type.BoolType | _ -> None

let type_of_len (container : Type.t) (ty : Type.t) : Type.t option =
  match (container, ty) with
  | (Type.ListType, Type.ListType) -> Some Type.IntType
  | (Type.TupleType, Type.TupleType) -> Some Type.IntType
  | _ -> None

let on_int ~(return : Type.t) = function
  | Type.IntType -> Some return
  | _ -> None

let on_float ~(return : Type.t) = function
  | Type.FltType -> Some return
  | _ -> None

let type_of_sconcat = function Type.ListType -> Some Type.StrType | _ -> None

let type_of_binary_arith (ty1 : Type.t) (ty2 : Type.t) : Type.t option =
  match (ty1, ty2) with
  | (Type.IntType, Type.IntType) -> Some Type.IntType
  | (Type.FltType, Type.IntType) -> Some Type.FltType
  | (Type.IntType, Type.FltType) -> Some Type.FltType
  | (Type.FltType, Type.FltType) -> Some Type.FltType
  | _ -> None

let type_of_eq (ty1 : Type.t) (ty2 : Type.t) : Type.t option =
  (* FIXME: This is useless? *)
  match (ty1, ty2) with
  | (Type.IntType, Type.IntType)
  | (Type.FltType, Type.FltType)
  | (Type.ArrayType, Type.ArrayType)
  | (Type.StrType, Type.StrType)
  | (Type.BoolType, Type.BoolType) ->
    Some Type.BoolType
  (* missing the symbolic cases *)
  | _ ->
    (* None *)
    Some Type.BoolType

let type_of_relop (ty1 : Type.t) (ty2 : Type.t) : Type.t option =
  match (ty1, ty2) with
  | (Type.IntType, Type.IntType) -> Some Type.BoolType
  | (Type.FltType, Type.FltType) -> Some Type.BoolType
  | _ -> None

let type_of_binary_logic (arg1_t : Type.t) (arg2_t : Type.t) : Type.t option =
  match (arg1_t, arg2_t) with
  | (Type.BoolType, Type.BoolType) -> Some Type.BoolType
  | _ -> None

let bitwise_operators_typing_logic (ty1 : Type.t) (ty2 : Type.t) : Type.t option
    =
  match (ty1, ty2) with
  | (Type.FltType, Type.FltType) -> Some Type.FltType
  | _ -> None

let bin_args_typing_inlist (arg1_t : Type.t) : Type.t option =
  match arg1_t with Type.ListType -> Some Type.BoolType | _ -> None

let bin_args_typing_lnth (arg1 : Value.value) (arg2 : Value.value) :
  Type.t option =
  let open Value in
  match (arg1, arg2) with
  | (Val (Val.List l), Val (Val.Int i)) -> Val.type_of (List.nth l i)
  | (_, _) -> None

let bin_args_typing_pow (arg1_t : Type.t) (arg2_t : Type.t) : Type.t option =
  match (arg1_t, arg2_t) with
  | (Type.FltType, Type.FltType) -> Some Type.FltType
  | _ -> None

let type_of_unop (op : Operator.unopt) (ty : Type.t) : Type.t option =
  match op with
  | Operator.Neg -> type_of_neg ty
  | Operator.LogicalNot -> type_of_not ty
  | Operator.BitwiseNot -> type_of_bitwise_not ty
  | Operator.ListHead -> None
  | Operator.ListTail -> None
  | Operator.TupleFirst -> None
  | Operator.TupleSecond -> None
  | Operator.TupleLen -> type_of_len Type.TupleType ty
  | Operator.IntToFloat -> on_int ~return:Type.FltType ty
  | Operator.IntToString -> on_int ~return:Type.StrType ty
  | Operator.FloatToInt -> on_float ~return:Type.IntType ty
  | Operator.FloatToString -> on_float ~return:Type.StrType ty
  | Operator.StringToFloat -> Some Type.FltType
  | Operator.ToUint32 -> Some Type.FltType
  | Operator.Typeof -> Some Type.TypeType
  | Operator.StringConcat -> type_of_sconcat ty
  | Operator.Exp -> on_float ~return:Type.FltType ty
  | Operator.LogE -> on_float ~return:Type.FltType ty
  | Operator.Log10 -> on_float ~return:Type.FltType ty
  | Operator.Ceil -> on_float ~return:Type.FltType ty
  | Operator.Floor -> on_float ~return:Type.FltType ty
  | Operator.Abs -> on_float ~return:Type.FltType ty
  | Operator.ToInt -> on_float ~return:Type.FltType ty
  | Operator.Sqrt -> on_float ~return:Type.FltType ty
  | Operator.IsNaN -> on_float ~return:Type.BoolType ty
  | Operator.StringLen -> Some Type.IntType
  | Operator.StringLenU -> Some Type.IntType
  | Operator.Trim -> Some Type.StrType
  | Operator.ListLen -> Some Type.IntType
  | Operator.ListSort -> Some Type.ListType
  | Operator.ToCharCodeU -> Some Type.StrType
  | _ ->
    Log.err
      "Typing Error: [type_of_unop] -> unsuported typing for unary operation %a"
      Operator.pp_of_unopt_single op

let type_of_binop (op : Operator.binopt) (v1 : Value.value) (v2 : Value.value)
  (ty1 : Type.t) (ty2 : Type.t) : Type.t option =
  match op with
  | Operator.Plus -> type_of_binary_arith ty1 ty2
  | Operator.Minus -> type_of_binary_arith ty1 ty2
  | Operator.Times -> type_of_binary_arith ty1 ty2
  | Operator.Div -> type_of_binary_arith ty1 ty2
  | Operator.Modulo -> type_of_binary_arith ty1 ty2
  | Operator.Eq -> type_of_eq ty1 ty2
  | Operator.Lt -> type_of_relop ty1 ty2
  | Operator.Gt -> type_of_relop ty1 ty2
  | Operator.Le -> type_of_relop ty1 ty2
  | Operator.Ge -> type_of_relop ty1 ty2
  | Operator.LogicalAnd -> type_of_binary_logic ty1 ty2
  | Operator.LogicalOr -> type_of_binary_logic ty1 ty2
  | Operator.Min -> type_of_binary_arith ty1 ty2
  | Operator.Max -> type_of_binary_arith ty1 ty2
  | Operator.TupleNth -> None
  | Operator.ListNth -> bin_args_typing_lnth v1 v2
  | Operator.ListMem -> bin_args_typing_inlist ty1
  | Operator.ListPrepend -> None
  | Operator.ListAdd -> None
  | Operator.Pow -> bin_args_typing_pow ty1 ty2
  | Operator.StringNth -> Some Type.StrType
  | Operator.StringNthU -> Some Type.StrType
  | Operator.BitwiseAnd | Operator.BitwiseOr | Operator.BitwiseXor
  | Operator.ShiftLeft | Operator.ShiftRight | Operator.ShiftRightLogical ->
    bitwise_operators_typing_logic ty1 ty2
  | _ ->
    Log.err "Typing Error: [type_of_binop] -> unsuported typing for %a"
      Operator.pp_of_binopt_single op

let type_of_triop (op : Operator.triopt) (_ : Type.t) (ty2 : Type.t)
  (ty3 : Type.t) : Type.t option =
  match op with
  | Operator.StringSubstr -> Some Type.StrType
  | Operator.StringSubstrU -> Some Type.StrType
  | Operator.ListSet -> Some Type.ListType
  | Operator.ArraySet -> Some Type.ArrayType
  | Operator.ITE -> (
    if ty2 = ty3 then Some ty2
    else
      match (ty2, ty3) with
      | (_, Type.SymbolType) -> Some ty2
      | (Type.SymbolType, _) -> Some ty3
      | _ ->
        Log.err "types don't match for ITE: %a %a@." Type.pp ty2 Type.pp ty3 )

let rec type_of (v : Symbolic_value.M.value) : Type.t option =
  let open Value in
  let open Syntax.Option in
  match v with
  | Val v -> Val.type_of v
  | UnOpt (op, v) ->
    let* ty = type_of v in
    type_of_unop op ty
  | BinOpt (op, v1, v2) ->
    let* ty1 = type_of v1 in
    let* ty2 = type_of v2 in
    type_of_binop op v1 v2 ty1 ty2
  | TriOpt (op, v1, v2, v3) ->
    let* ty1 = type_of v1 in
    let* ty2 = type_of v2 in
    let* ty3 = type_of v3 in
    type_of_triop op ty1 ty2 ty3
  | Symbolic (t, _) -> Some t
  | NOpt (Operator.ListExpr, _) -> Some Type.ListType
  | NOpt (Operator.TupleExpr, _) -> Some Type.TupleType
  | NOpt (Operator.ArrayExpr, _) -> Some Type.ArrayType
  | Curry _ -> Some Type.CurryType
  | _ -> Log.err "%a: Not typed!" pp v
