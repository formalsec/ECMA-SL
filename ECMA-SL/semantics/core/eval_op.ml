open Operators

let eval_unop (op : uopt) (v : Val.t) : Val.t =
  match op with
  | Neg -> neg v
  | LogicalNot -> not v
  | IsNaN -> is_NaN v
  | BitwiseNot -> bitwise_not v
  | Typeof -> typeof v
  | ListLen -> l_len v
  | TupleLen -> t_len v
  | StringLen -> s_len v
  | StringLenU -> s_len_u v
  | ListHead -> head v
  | ListTail -> tail v
  | TupleFirst -> first v
  | TupleSecond -> second v
  | ListRemoveLast -> list_remove_last v
  | ListSort -> list_sort v
  | ListReverse -> list_reverse v
  | IntToFloat -> int_to_float v
  | IntToString -> int_to_string v
  | IntToFourHex -> int_to_four_hex v
  | StringToInt -> int_of_string v
  | FloatToInt -> int_of_float v
  | FloatToString -> float_to_string v
  | StringToFloat -> float_of_string v
  | HexDecode -> hex_decode v
  | Utf8Decode -> utf8_decode v
  | OctalToDecimal -> octal_to_decimal v
  | StringConcat -> string_concat v
  | ObjectToList ->
    raise (Failure "Unexpected call to eval_unop with operator ObjToList")
  | ObjectFields ->
    raise (Failure "Unexpected call to eval_unop with operator ObjFields")
  | ToInt -> to_int v
  | ToInt32 -> to_int32 v
  | ToUint32 -> to_uint32 v
  | FromCharCode -> from_char_code v
  | FromCharCodeU -> from_char_code_u v
  | ToCharCode -> to_char_code v
  | ToCharCodeU -> to_char_code_u v
  | ToLowerCase -> to_lower_case v
  | ToUpperCase -> to_upper_case v
  | Trim -> trim v
  | ToUint16 -> to_uint16 v
  | ParseNumber -> parse_number v
  | ParseString -> parse_string v
  | ParseDate -> parse_date v
  | Log2 -> log_2 v
  | Float64ToLEBytes -> float64_to_le_bytes v
  | Float64ToBEBytes -> float64_to_be_bytes v
  | Float32ToLEBytes -> float32_to_le_bytes v
  | Float32ToBEBytes -> float32_to_be_bytes v
  | Float64FromLEBytes -> float64_from_le_bytes v
  | Float64FromBEBytes -> float64_from_be_bytes v
  | Float32FromLEBytes -> float32_from_le_bytes v
  | Float32FromBEBytes -> float32_from_be_bytes v
  | BytesToString -> bytes_to_string v
  | FloatToByte -> float_to_byte v
  | ArrayLen -> a_len v
  | ListToArray -> list_to_array v
  | _ -> apply_uopt_oper op v

let eval_binopt_expr (op : bopt) (v1 : Val.t) (v2 : Val.t) : Val.t =
  match op with
  | Plus -> plus (v1, v2)
  | Minus -> minus (v1, v2)
  | Times -> times (v1, v2)
  | Div -> div (v1, v2)
  | Modulo -> modulo (v1, v2)
  | Eq -> equal (v1, v2)
  | Gt -> gt (v1, v2)
  | Lt -> lt (v1, v2)
  | Ge -> egt (v1, v2)
  | Le -> elt (v1, v2)
  | LogicalAnd -> log_and (v1, v2)
  | LogicalOr -> log_or (v1, v2)
  | BitwiseAnd -> bitwise_and (v1, v2)
  | BitwiseOr -> bitwise_or (v1, v2)
  | BitwiseXor -> bitwise_xor (v1, v2)
  | ShiftLeft -> shift_left (v1, v2)
  | ShiftRight -> shift_right (v1, v2)
  | ShiftRightLogical -> shift_right_logical (v1, v2)
  | ListNth -> list_nth (v1, v2)
  | ListRemoveNth -> list_remove_nth (v1, v2)
  | ListRemove -> list_remove (v1, v2)
  | TupleNth -> tuple_nth (v1, v2)
  | StringNth -> s_nth (v1, v2)
  | StringNthU -> s_nth_u (v1, v2)
  | StringSplit -> string_split (v1, v2)
  | ListAdd -> list_add (v1, v2)
  | ListPrepend -> list_prepend (v1, v2)
  | ListConcat -> list_concat (v1, v2)
  | ListMem -> list_in (v1, v2)
  | ObjectMem -> raise (Failure "Not expected")
  | ToPrecision -> to_precision (v1, v2)
  | ToExponential -> to_exponential (v1, v2)
  | ToFixed -> to_fixed (v1, v2)
  | ArrayMake -> array_make (v1, v2)
  | ArrayNth -> array_nth (v1, v2)
  | IntToBEBytes -> int_to_be_bytes (v1, v2)
  | IntFromLEBytes -> int_from_le_bytes (v1, v2)
  | UintFromLEBytes -> uint_from_le_bytes (v1, v2)
  | _ -> apply_bopt_oper op v1 v2

let eval_triopt_expr (op : topt) (v1 : Val.t) (v2 : Val.t) (v3 : Val.t) : Val.t
    =
  match op with
  | StringSubstr -> s_substr (v1, v2, v3)
  | StringSubstrU -> s_substr_u (v1, v2, v3)
  | ArraySet -> array_set (v1, v2, v3)
  | ListSet -> list_set (v1, v2, v3)
  | ITE -> ite (v1, v2, v3)

let eval_nopt_expr (op : nopt) (vals : Val.t list) : Val.t =
  match op with
  | NAryLogicalAnd -> Val.Bool (List.for_all is_true vals)
  | NAryLogicalOr -> Val.Bool (List.exists is_true vals)
  | ArrayExpr -> Val.Arr (Array.of_list vals)
  | ListExpr -> Val.List vals
  | TupleExpr -> Val.Tuple vals
