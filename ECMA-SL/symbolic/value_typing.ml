open Args_typing_unop
open Args_typing_binop
open Type_of_val

let type_of_unop (op : Operator.unopt) (arg_t : Type.t option) : Type.t option =
  match op with
  | Operator.Neg -> un_args_typing_neg arg_t
  | Operator.LogicalNot -> un_args_typing_not arg_t
  | Operator.ListHead -> None
  | Operator.ListTail -> None
  | Operator.TupleFirst -> None
  | Operator.TupleSecond -> None
  | Operator.TupleLen -> un_args_typing_container_length arg_t Type.TupleType
  | Operator.IntToFloat -> un_args_typing_int_casts arg_t Type.FltType
  | Operator.IntToString -> un_args_typing_int_casts arg_t Type.StrType
  | Operator.FloatToString -> un_args_typing_float_casts arg_t Type.StrType
  | Operator.StringToFloat -> Some Type.FltType
  | Operator.ToUint32 -> Some Type.FltType
  | Operator.Typeof -> un_args_typing_typeof arg_t
  | Operator.StringConcat -> un_args_typing_sconcat arg_t
  | Operator.Exp -> un_args_typing_float_math arg_t Type.FltType
  | Operator.LogE -> un_args_typing_float_math arg_t Type.FltType
  | Operator.Log10 -> un_args_typing_float_math arg_t Type.FltType
  | Operator.Ceil -> un_args_typing_float_math arg_t Type.FltType
  | Operator.Floor -> un_args_typing_float_math arg_t Type.FltType
  | Operator.Abs -> un_args_typing_float_math arg_t Type.FltType
  | Operator.ToInt -> un_args_typing_float_math arg_t Type.FltType
  | Operator.Sqrt -> un_args_typing_float_math arg_t Type.FltType
  | Operator.IsNaN -> un_args_typing_float_math arg_t Type.BoolType
  | Operator.StringLen -> Some Type.IntType
  | Operator.StringLenU -> Some Type.IntType
  | Operator.BitwiseNot -> un_args_typing_bitwise_not arg_t
  | Operator.Trim -> Some Type.StrType
  | Operator.ListLen -> Some Type.IntType
  | Operator.ListSort -> Some Type.ListType
  | _ ->
    failwith
      ( "Typing Error: [type_of_unop] -> unsuported typing for unary operation "
      ^ Operator.str_of_unopt_single op )

let type_of_binop (op : Operator.binopt) (arg1 : Symbolic_value.M.value)
  (arg2 : Symbolic_value.M.value) (arg1_t : Type.t option)
  (arg2_t : Type.t option) : Type.t option =
  match op with
  | Operator.Plus -> bin_args_typing_arith arg1_t arg2_t
  | Operator.Minus -> bin_args_typing_arith arg1_t arg2_t
  | Operator.Times -> bin_args_typing_arith arg1_t arg2_t
  | Operator.Div -> bin_args_typing_arith arg1_t arg2_t
  | Operator.Modulo -> bin_args_typing_arith arg1_t arg2_t
  | Operator.Eq -> bin_args_typing_eq arg1_t arg2_t
  | Operator.Lt -> bin_args_typing_comp arg1_t arg2_t
  | Operator.Gt -> bin_args_typing_comp arg1_t arg2_t
  | Operator.Le -> bin_args_typing_comp arg1_t arg2_t
  | Operator.Ge -> bin_args_typing_comp arg1_t arg2_t
  | Operator.LogicalAnd -> bin_args_typing_logic arg1_t arg2_t
  | Operator.LogicalOr -> bin_args_typing_logic arg1_t arg2_t
  | Operator.Min -> bin_args_typing_arith arg1_t arg2_t
  | Operator.Max -> bin_args_typing_arith arg1_t arg2_t
  | Operator.TupleNth -> None
  | Operator.ListNth -> bin_args_typing_lnth arg1 arg2
  | Operator.ListMem -> bin_args_typing_inlist arg1_t
  | Operator.ListPrepend -> None
  | Operator.ListAdd -> None
  | Operator.Pow -> bin_args_typing_pow arg1_t arg2_t
  | Operator.StringNth -> Some Type.StrType
  | Operator.StringNthU -> Some Type.StrType
  | Operator.BitwiseAnd | Operator.BitwiseOr | Operator.BitwiseXor
  | Operator.ShiftLeft | Operator.ShiftRight | Operator.ShiftRightLogical ->
    bitwise_operators_typing_logic arg1_t arg2_t
  | _ ->
    failwith
      ( "Typing Error: [type_of_binop] -> unsuported typing for binary \
         operation "
      ^ Operator.str_of_binopt_single op )

let type_of_triop (op : Operator.triopt) (_arg1_t : Type.t option)
  (arg2_t : Type.t option) (arg3_t : Type.t option) : Type.t option =
  match op with
  | Operator.StringSubstr -> Some Type.StrType
  | Operator.StringSubstrU -> Some Type.StrType
  | Operator.ListSet -> Some Type.ListType
  | Operator.ArraySet -> Some Type.ArrayType
  | Operator.ITE -> (
    match (arg2_t, arg3_t) with
    | (Some t2, Some t3) -> (
      if t2 = t3 then arg2_t
      else
        match (t2, t3) with
        | (_, Type.SymbolType) -> arg2_t
        | (Type.SymbolType, _) -> arg3_t
        | _ ->
          Format.kasprintf failwith "types don't match for ITE: %s %s\n"
            (Type.str t2) (Type.str t3) )
    | _ -> failwith "types don't match for ITE." )

let rec type_of (v : Symbolic_value.M.value) : Type.t option =
  let open Symbolic_value.M in
  match v with
  | Val v -> type_of_val v
  | UnOpt (op, arg) ->
    let arg_t = type_of arg in
    type_of_unop op arg_t
  | BinOpt (op, arg1, arg2) ->
    let arg1_t = type_of arg1
    and arg2_t = type_of arg2 in
    type_of_binop op arg1 arg2 arg1_t arg2_t
  | TriOpt (op, arg1, arg2, arg3) ->
    let arg1_t = type_of arg1
    and arg2_t = type_of arg2
    and arg3_t = type_of arg3 in
    type_of_triop op arg1_t arg2_t arg3_t
  | Symbolic (t, _) -> Some t
  | NOpt (Operator.ListExpr, _) -> Some Type.ListType
  | NOpt (Operator.TupleExpr, _) -> Some Type.TupleType
  | NOpt (Operator.ArrayExpr, _) -> Some Type.ArrayType
  | Curry _ -> Some Type.CurryType
  | _ -> Format.kasprintf failwith "%a: Not typed!" Pp.pp v
