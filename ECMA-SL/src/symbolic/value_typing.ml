open Args_typing_unop
open Args_typing_binop
open Type_of_val

let type_of_unop (op : Operators.uopt) (arg_t : Type.t option) : Type.t option =
  match op with
  | Operators.Neg -> un_args_typing_neg arg_t
  | Operators.Not -> un_args_typing_not arg_t
  | Operators.Head -> None
  | Operators.Tail -> None
  | Operators.First -> None
  | Operators.Second -> None
  | Operators.TupleLen -> un_args_typing_container_length arg_t Type.TupleType
  | Operators.IntToFloat -> un_args_typing_int_casts arg_t Type.FltType
  | Operators.IntToString -> un_args_typing_int_casts arg_t Type.StrType
  | Operators.FloatToString -> un_args_typing_float_casts arg_t Type.StrType
  | Operators.FloatOfString -> Some Type.FltType
  | Operators.ToUint32 -> Some Type.FltType
  | Operators.Typeof -> un_args_typing_typeof arg_t
  | Operators.Sconcat -> un_args_typing_sconcat arg_t
  | Operators.Exp -> un_args_typing_float_math arg_t Type.FltType
  | Operators.Log_e -> un_args_typing_float_math arg_t Type.FltType
  | Operators.Log_10 -> un_args_typing_float_math arg_t Type.FltType
  | Operators.Ceil -> un_args_typing_float_math arg_t Type.FltType
  | Operators.Floor -> un_args_typing_float_math arg_t Type.FltType
  | Operators.Abs -> un_args_typing_float_math arg_t Type.FltType
  | Operators.ToInt -> un_args_typing_float_math arg_t Type.FltType
  | Operators.Sqrt -> un_args_typing_float_math arg_t Type.FltType
  | Operators.IsNaN -> un_args_typing_float_math arg_t Type.BoolType
  | Operators.StringLen -> Some Type.IntType
  | Operators.StringLenU -> Some Type.IntType
  | Operators.BitwiseNot -> un_args_typing_bitwise_not arg_t
  | Operators.Trim -> Some Type.StrType
  | Operators.ListLen -> Some Type.IntType
  | Operators.LSort -> Some Type.ListType
  | _ ->
      failwith
        ("Typing Error: [type_of_unop] -> unsuported typing for unary \
          operation " ^ Operators.str_of_unopt op)

let type_of_binop (op : Operators.bopt) (arg1 : Sym_value.M.value)
    (arg2 : Sym_value.M.value) (arg1_t : Type.t option) (arg2_t : Type.t option)
    : Type.t option =
  match op with
  | Operators.Plus -> bin_args_typing_arith arg1_t arg2_t
  | Operators.Minus -> bin_args_typing_arith arg1_t arg2_t
  | Operators.Times -> bin_args_typing_arith arg1_t arg2_t
  | Operators.Div -> bin_args_typing_arith arg1_t arg2_t
  | Operators.Modulo -> bin_args_typing_arith arg1_t arg2_t
  | Operators.Eq -> bin_args_typing_eq arg1_t arg2_t
  | Operators.Lt -> bin_args_typing_comp arg1_t arg2_t
  | Operators.Gt -> bin_args_typing_comp arg1_t arg2_t
  | Operators.Le -> bin_args_typing_comp arg1_t arg2_t
  | Operators.Ge -> bin_args_typing_comp arg1_t arg2_t
  | Operators.Log_And -> bin_args_typing_logic arg1_t arg2_t
  | Operators.Log_Or -> bin_args_typing_logic arg1_t arg2_t
  | Operators.Min -> bin_args_typing_arith arg1_t arg2_t
  | Operators.Max -> bin_args_typing_arith arg1_t arg2_t
  | Operators.Tnth -> None
  | Operators.Lnth -> bin_args_typing_lnth arg1 arg2
  | Operators.InList -> bin_args_typing_inlist arg1_t
  | Operators.Lprepend -> None
  | Operators.Ladd -> None
  | Operators.Pow -> bin_args_typing_pow arg1_t arg2_t
  | Operators.Snth -> Some Type.StrType
  | Operators.Snth_u -> Some Type.StrType
  | Operators.BitwiseAnd | Operators.BitwiseOr | Operators.BitwiseXor
  | Operators.ShiftLeft | Operators.ShiftRight | Operators.ShiftRightLogical ->
      bitwise_operators_typing_logic arg1_t arg2_t
  | _ ->
      failwith
        ("Typing Error: [type_of_binop] -> unsuported typing for binary \
          operation "
        ^ Operators.str_of_binopt_single op)

let type_of_triop (op : Operators.topt) (_arg1_t : Type.t option)
    (arg2_t : Type.t option) (arg3_t : Type.t option) : Type.t option =
  match op with
  | Operators.Ssubstr -> Some Type.StrType
  | Operators.SsubstrU -> Some Type.StrType
  | Operators.Lset -> Some Type.ListType
  | Operators.Aset -> Some Type.ArrayType
  | Operators.ITE -> (
      match (arg2_t, arg3_t) with
      | Some t2, Some t3 ->
          if t2 = t3 then arg2_t else failwith "types don't match for ITE."
      | _ -> failwith "types don't match for ITE.")

let rec type_of (v : Sym_value.M.value) : Type.t option =
  let open Sym_value.M in
  match v with
  | Val v -> type_of_val v
  | UnOpt (op, arg) ->
      let arg_t = type_of arg in
      type_of_unop op arg_t
  | BinOpt (op, arg1, arg2) ->
      let arg1_t = type_of arg1 and arg2_t = type_of arg2 in
      type_of_binop op arg1 arg2 arg1_t arg2_t
  | TriOpt (op, arg1, arg2, arg3) ->
      let arg1_t = type_of arg1
      and arg2_t = type_of arg2
      and arg3_t = type_of arg3 in
      type_of_triop op arg1_t arg2_t arg3_t
  | Symbolic (t, _) -> Some t
  | NOpt (Operators.ListExpr, _) -> Some Type.ListType
  | NOpt (Operators.TupleExpr, _) -> Some Type.TupleType
  | NOpt (Operators.ArrExpr, _) -> Some Type.ArrayType
  | _ -> failwith (Pp.pp v ^ ": Not typed!")
