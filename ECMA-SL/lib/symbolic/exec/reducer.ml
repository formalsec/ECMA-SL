open Core
open Expr
open Val
open Operators

let reduce_unop (op : uopt) (v : Expr.t) : Expr.t =
  match (op, v) with
  | Neg, Val (Flt f) -> Val (Flt (-.f))
  | Neg, Val (Int i) -> Val (Int (-i))
  | Neg, Val v' -> UnOpt (Neg, v)
  | Neg, Symbolic (_, _) -> UnOpt (Neg, v)
  | Not, Val (Bool b) -> Val (Bool (Caml.Bool.not b))
  | Not, v' -> UnOpt (Not, v)
  | Head, NOpt (ListExpr, a :: _) -> a
  | Tail, NOpt (ListExpr, _ :: tl) -> NOpt (ListExpr, tl)
  | First, NOpt (TupleExpr, a :: _) -> a
  | Second, NOpt (TupleExpr, _ :: b :: _) -> b
  | ListLen, NOpt (ListExpr, vs) -> Val (Int (List.length vs))
  | TupleLen, NOpt (TupleExpr, vs) -> Val (Int (List.length vs))
  | IntToFloat, Val (Int i) -> Val (Flt (Float.of_int i))
  | IntToString, Val (Int i) -> Val (Str (Int.to_string i))
  | FloatToString, Val (Flt f) -> Val (Str (Float.to_string f))
  | FloatToString, Symbolic (_, _) -> UnOpt (FloatToString, v)
  | Typeof, v -> failwith "TODO"
  | Sconcat, NOpt (ListExpr, vs) ->
      Val
        (Str
           (String.concat ~sep:""
              (List.fold_left vs ~init:[] ~f:(fun a b ->
                   match b with Val (Str s) -> a @ [ s ] | _ -> a))))
  | Trim, Val (Str s) -> Val (Str (String_utils.trim s))
  | StringLen, Val (Str s) -> Val (Int (String.length s))
  | StringLen, Symbolic (Type.StrType, _) -> UnOpt (StringLen, v)
  | StringLenU, Val (Str s) -> Val (Int (String_utils.s_len_u s))
  | FloatOfString, Val (Str s) ->
      let trimmed = String.strip s in
      Val
        (if String.length trimmed = 0 then Flt Float.nan
        else try Flt (Float.of_string trimmed) with exn -> Flt Float.nan)
  | FloatOfString, UnOpt (FloatToString, Symbolic (t, x)) -> Symbolic (t, x)
  | Exp, Val (Flt f) -> Val (Flt (Float.exp f))
  | Log_e, Val (Flt f) -> Val (Flt (Float.log f))
  | Log_10, Val (Flt f) -> Val (Flt (Float.log10 f))
  | Sqrt, Val (Flt f) -> Val (Flt (Float.sqrt f))
  | Abs, Val (Flt f) -> Val (Flt (Float.abs f))
  | Floor, Val (Flt f) -> Val (Flt (Float.round_down f))
  | ToInt, Val (Flt f) -> Val (Flt (Arith_utils.to_int f))
  | ToUint32, Val (Flt f) -> Val (Flt (Arith_utils.to_uint32 f))
  | IsNaN, Val (Flt n) -> Val (Bool (Float.is_nan n))
  | IsNaN, v' -> BinOpt (Eq, v', Val (Flt Float.nan))
  | BitwiseNot, Val (Flt f) -> Val (Flt (Arith_utils.int32_bitwise_not f))
  | ListLen, Val (List l) -> Val (Val.Int (List.length l))
  | TupleLen, Val (Tuple t) -> Val (Val.Int (List.length t))
  | Head, Val v' -> Val (Operators.head v')
  | Tail, Val v' -> Val (Operators.tail v')
  | First, Val v' -> Val (Operators.first v')
  | Second, Val v' -> Val (Operators.second v')
  | LRemoveLast, Val v' -> Val (Operators.list_remove_last v')
  | LSort, Val v' -> Val (Operators.list_sort v')
  | LReverse, Val v' -> Val (Operators.list_reverse v')
  | IntToFourHex, Val (Int i) -> Val (Str (Printf.sprintf "%04x" i))
  | IntToFourHex, Symbolic (Type.IntType, _) -> UnOpt (op, v)
  | IntOfString, Val v' -> Val (Operators.int_of_string v')
  | IntOfString, Symbolic (Type.StrType, _) -> UnOpt (op, v)
  | IntOfFloat, Val v' -> Val (Operators.int_of_float v')
  | IntOfFloat, Symbolic (Type.FltType, _) -> UnOpt (op, v)
  | HexDecode, Val v' -> Val (Operators.hex_decode v')
  | HexDecode, Symbolic (Type.StrType, _) -> UnOpt (op, v)
  | Utf8Decode, Val v' -> Val (Operators.utf8_decode v')
  | Utf8Decode, Symbolic (Type.StrType, _) -> UnOpt (op, v)
  | OctalToDecimal, Val v' -> Val (Operators.octal_to_decimal v')
  | OctalToDecimal, Symbolic (Type.IntType, _) -> UnOpt (op, v)
  | Sconcat, Val v' -> Val (Operators.string_concat v')
  | ToInt, Val v' -> Val (Operators.to_int v')
  | ToInt, Symbolic (Type.FltType, _) -> UnOpt (op, v)
  | ToInt32, Val v' -> Val (Operators.to_int32 v')
  | ToInt32, Symbolic (Type.FltType, _) -> UnOpt (op, v)
  | ToUint16, Val v' -> Val (Operators.to_uint16 v')
  | ToUint16, Symbolic (Type.FltType, _) -> UnOpt (op, v)
  | ToUint32, Val v' -> Val (Operators.to_uint32 v')
  | ToUint32, Symbolic (Type.FltType, _) -> UnOpt (op, v)
  | FromCharCode, Val v' -> Val (Operators.from_char_code v')
  | FromCharCode, Symbolic (Type.IntType, _) -> UnOpt (op, v)
  | FromCharCodeU, Val v' -> Val (Operators.from_char_code_u v')
  | FromCharCodeU, Symbolic (Type.IntType, _) -> UnOpt (op, v)
  | ToCharCode, Val v' -> Val (Operators.to_char_code v')
  | ToCharCode, Symbolic (Type.StrType, _) -> UnOpt (op, v)
  | ToCharCodeU, Val v' -> Val (Operators.to_char_code_u v')
  | ToCharCodeU, Symbolic (Type.StrType, _) -> UnOpt (op, v)
  | ToLowerCase, Val v' -> Val (Operators.to_lower_case v')
  | ToLowerCase, Symbolic (Type.StrType, _) -> UnOpt (op, v)
  | ToUpperCase, Val v' -> Val (Operators.to_upper_case v')
  | ToUpperCase, Symbolic (Type.StrType, _) -> UnOpt (op, v)
  | Trim, Val v' -> Val (Operators.trim v')
  | Trim, Symbolic (Type.StrType, _) -> UnOpt (op, v)
  | ParseNumber, Val v' -> Val (Operators.parse_number v')
  | ParseNumber, Symbolic (Type.StrType, _) -> UnOpt (op, v)
  | ParseString, Val v' -> Val (Operators.parse_string v')
  | ParseDate, Val v' -> Val (Operators.parse_date v')
  | Float64ToLEBytes, Val v' -> Val (Operators.float64_to_le_bytes v')
  | Float64ToBEBytes, Val v' -> Val (Operators.float64_to_be_bytes v')
  | Float32ToLEBytes, Val v' -> Val (Operators.float32_to_le_bytes v')
  | Float32ToBEBytes, Val v' -> Val (Operators.float32_to_be_bytes v')
  | Float64FromLEBytes, Val v' -> Val (Operators.float64_from_le_bytes v')
  | Float64FromBEBytes, Val v' -> Val (Operators.float64_from_be_bytes v')
  | Float32FromLEBytes, Val v' -> Val (Operators.float32_from_le_bytes v')
  | Float32FromBEBytes, Val v' -> Val (Operators.float32_from_be_bytes v')
  | BytesToString, Val v' -> Val (Operators.bytes_to_string v')
  | FloatToByte, Val v' -> Val (Operators.float_to_byte v')
  | ArrayLen, Val v' -> Val (Operators.a_len v')
  | ListToArray, Val v' -> Val (Operators.list_to_array v')
  | Acos, Val v'
  | Asin, Val v'
  | Atan, Val v'
  | Ceil, Val v'
  | Cos, Val v'
  | Random, Val v'
  | Sin, Val v'
  | Tan, Val v'
  | Tanh, Val v'
  | Sinh, Val v'
  | Cosh, Val v'
  | Log_2, Val v' ->
      Val (Operators.apply_uopt_oper op v')
  (* missing obj_to_list, obj_fields*)
  | _ ->
      (* TODO: clear error message *)
      invalid_arg
        ("ill-typed or not implemented UnOpt: '" ^ str_of_unopt op ^ "'")

let reduce_binop (op : bopt) (v1 : Expr.t) (v2 : Expr.t) : Expr.t =
  match (op, v1, v2) with
  | Plus, Val (Int i1), Val (Int i2) -> Val (Int (i1 + i2))
  | Plus, Val (Flt f1), Val (Flt f2) -> Val (Flt (f1 +. f2))
  | Minus, Val (Int i1), Val (Int i2) -> Val (Int (i1 - i2))
  | Minus, Val (Flt f1), Val (Flt f2) -> Val (Flt (f1 -. f2))
  | Times, Val (Int i1), Val (Int i2) -> Val (Int (i1 * i2))
  | Times, Val (Flt f1), Val (Flt f2) -> Val (Flt (f1 *. f2))
  | Div, Val (Int i1), Val (Int i2) -> Val (Int (i1 / i2))
  | Div, Val (Flt f1), Val (Flt f2) -> Val (Flt (f1 /. f2))
  | Modulo, Val (Flt f1), Val (Flt f2) -> Val (Flt (Float.mod_float f1 f2))
  | Eq, Val (Flt f1), Val (Flt f2) -> Val (Bool (Float.equal f1 f2))
  | Eq, Val (Arr a1), Val (Arr a2) -> Val (Bool (Caml.( == ) a1 a2))
  | Eq, v', Val Null when is_symbolic v' -> Val (Bool false)
  | Eq, Val v', Val (Symbol _) when Caml.not (is_symbol v') -> Val (Bool false)
  | Eq, _, _ when Caml.not (is_symbolic v1 || is_symbolic v2) ->
      Val (Bool (Caml.( = ) v1 v2))
  | Lt, Val (Int i1), Val (Int i2) -> Val (Bool (i1 < i2))
  | Lt, Val (Flt f1), Val (Flt f2) -> Val (Bool Float.(f1 < f2))
  | Gt, Val (Int i1), Val (Int i2) -> Val (Bool (i1 > i2))
  | Gt, Val (Flt f1), Val (Flt f2) -> Val (Bool Float.(f1 > f2))
  | Le, Val (Int i1), Val (Int i2) -> Val (Bool (i1 <= i2))
  | Le, Val (Flt f1), Val (Flt f2) -> Val (Bool Float.(f1 <= f2))
  | Ge, Val (Int i1), Val (Int i2) -> Val (Bool (i1 >= i2))
  | Ge, Val (Flt f1), Val (Flt f2) -> Val (Bool Float.(f1 >= f2))
  | Min, Val (Flt f1), Val (Flt f2) -> Val (Flt (Float.min f1 f2))
  | Max, Val (Flt f1), Val (Flt f2) -> Val (Flt (Float.max f1 f2))
  | Pow, Val (Flt f1), Val (Flt f2) -> Val (Flt Float.(f1 ** f2))
  | Log_And, Val (Bool b1), Val (Bool b2) -> Val (Bool (b1 && b2))
  | Log_Or, Val (Bool b1), Val (Bool b2) -> Val (Bool (b1 || b2))
  | Tnth, NOpt (TupleExpr, vs), Val (Int i) -> List.nth_exn vs i
  | Lnth, NOpt (ListExpr, vs), Val (Int i) -> List.nth_exn vs i
  | InList, v1, NOpt (ListExpr, vs) -> Val (Bool (Caml.List.mem v1 vs))
  | Lprepend, v1, NOpt (ListExpr, vs) -> NOpt (ListExpr, v1 :: vs)
  | Ladd, NOpt (ListExpr, vs), v2 -> NOpt (ListExpr, vs @ [ v2 ])
  | Snth, Val (Str s), Val (Int i) ->
      Val (Str (String.nget s i |> String.of_char))
  | BitwiseAnd, Val v1, Val v2 -> Val (Operators.bitwise_and (v1, v2))
  | BitwiseOr, Val v1, Val v2 -> Val (Operators.bitwise_or (v1, v2))
  | BitwiseXor, Val v1, Val v2 -> Val (Operators.bitwise_xor (v1, v2))
  | ShiftLeft, Val v1, Val v2 -> Val (Operators.shift_left (v1, v2))
  | ShiftRight, Val v1, Val v2 -> Val (Operators.shift_right (v1, v2))
  | ShiftRightLogical, Val v1, Val v2 ->
      Val (Operators.shift_right_logical (v1, v2))
  | LRem, Val v1, Val v2 -> Val (Operators.list_remove (v1, v2))
  | LRemNth, Val v1, Val v2 -> Val (Operators.list_remove_nth (v1, v2))
  | Snth_u, Val v1, Val v2 -> Val (Operators.s_nth_u (v1, v2))
  | Ssplit, Val v1, Val v2 -> Val (Operators.string_split (v1, v2))
  | Ladd, Val v1, Val v2 -> Val (Operators.list_add (v1, v2))
  | Lprepend, Val v1, Val v2 -> Val (Operators.list_prepend (v1, v2))
  | Lconcat, Val v1, Val v2 -> Val (Operators.list_concat (v1, v2))
  | Atan2, Val v1, Val v2 -> Val (Operators.apply_bopt_oper op v1 v2)
  | ToPrecision, Val v1, Val v2 -> Val (Operators.to_precision (v1, v2))
  | ToExponential, Val v1, Val v2 -> Val (Operators.to_exponential (v1, v2))
  | ToFixed, Val v1, Val v2 -> Val (Operators.to_fixed (v1, v2))
  | ArrayMake, Val v1, Val v2 -> Val (Operators.array_make (v1, v2))
  | Anth, Val v1, Val v2 -> Val (Operators.array_nth (v1, v2))
  | IntToBEBytes, Val v1, Val v2 -> Val (Operators.int_to_be_bytes (v1, v2))
  | IntFromBytes, Val v1, Val v2 -> Val (Operators.int_from_le_bytes (v1, v2))
  | UintFromBytes, Val v1, Val v2 -> Val (Operators.uint_from_le_bytes (v1, v2))
  (*missing InObj | InList*)
  | op', v1', v2' -> BinOpt (op', v1', v2')

let reduce_triop (op : topt) (v1 : Expr.t) (v2 : Expr.t) (v3 : Expr.t) : Expr.t
    =
  TriOpt (op, v1, v2, v3)

let reduce_nop (op : nopt) (vs : Expr.t list) : Expr.t = NOpt (op, vs)
