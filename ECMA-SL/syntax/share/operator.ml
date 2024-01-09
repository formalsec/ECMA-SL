type 'a pp_fmt = Format.formatter -> 'a -> unit

type const =
  | MAX_VALUE
  | MIN_VALUE
  | PI

type unopt =
  | Typeof
  (* Arithmetic Operators *)
  | Neg
  (* Bitwise Operators *)
  | BitwiseNot
  (* Logical *)
  | LogicalNot
  (* Integer Operators *)
  | IntToFloat
  | IntToString
  | IntToFourHex
  | OctalToDecimal
  (* Float Operators *)
  | FloatToInt
  | FloatToString
  | ToInt
  | ToInt32
  | ToUint16
  | ToUint32
  | IsNaN
  (* String Operators *)
  | StringToInt
  | StringToFloat
  | FromCharCode
  | FromCharCodeU
  | ToCharCode
  | ToCharCodeU
  | ToLowerCase
  | ToUpperCase
  | Trim
  | StringLen
  | StringLenU
  | StringConcat
  (* Object Operators *)
  | ObjectToList
  | ObjectFields
  (* Array Operators *)
  | ArrayLen
  (* List Operators *)
  | ListToArray
  | ListHead
  | ListTail
  | ListLen
  | ListSort
  | ListReverse
  | ListRemoveLast
  (* Tuple Operators *)
  | TupleFirst
  | TupleSecond
  | TupleLen
  (* Byte Operators *)
  | FloatToByte
  | Float32ToLEBytes
  | Float32ToBEBytes
  | Float64ToLEBytes
  | Float64ToBEBytes
  | Float32FromLEBytes
  | Float32FromBEBytes
  | Float64FromLEBytes
  | Float64FromBEBytes
  | BytesToString
  (* Math Operators *)
  | Random
  | Abs
  | Sqrt
  | Ceil
  | Floor
  | Exp
  | Log2
  | LogE
  | Log10
  | Sin
  | Cos
  | Tan
  | Sinh
  | Cosh
  | Tanh
  | Asin
  | Acos
  | Atan
  (* Parse Operators *)
  | Utf8Decode
  | HexDecode
  | ParseNumber
  | ParseString
  | ParseDate

type binopt =
  (* Arithmetic Operators *)
  | Plus
  | Minus
  | Times
  | Div
  | Modulo
  | Pow
  (* Bitwise Operators *)
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | ShiftLeft
  | ShiftRight
  | ShiftRightLogical
  (* Logical Operators *)
  | LogicalAnd
  | LogicalOr
  (* Comparison Operators *)
  | Eq
  | Lt
  | Gt
  | Le
  | Ge
  (* Float Operators *)
  | ToPrecision
  | ToExponential
  | ToFixed
  (* Object Operators *)
  | ObjectMem
  (* String Operators *)
  | StringNth
  | StringNthU
  | StringSplit
  (* Array Operators *)
  | ArrayMake
  | ArrayNth
  (* List Operators *)
  | ListMem
  | ListNth
  | ListAdd
  | ListPrepend
  | ListConcat
  | ListRemove
  | ListRemoveNth
  (* Tuple Operators *)
  | TupleNth
  (* Byte Operators *)
  | IntToBEBytes
  | IntFromLEBytes
  | UintFromLEBytes
  (* Math Operators *)
  | Min
  | Max
  | Atan2

type triopt =
  (* String Operators *)
  | StringSubstr
  | StringSubstrU
  (* Array Operators *)
  | ArraySet
  (* List Operators *)
  | ListSet
  (* Other Operators *)
  | ITE

type nopt =
  (* Logical Operators *)
  | NAryLogicalAnd
  | NAryLogicalOr
  (* Array Operators *)
  | ArrayExpr
  (* List Operators *)
  | ListExpr
  (* Tuple Operators *)
  | TupleExpr

let is_infix_unopt (op : unopt) : bool =
  match op with BitwiseNot | LogicalNot -> true | _ -> false

let is_infix_binopt (op : binopt) : bool =
  match op with
  | Plus | Minus | Times | Div | Modulo | Pow | BitwiseAnd | BitwiseOr
  | BitwiseXor | ShiftLeft | ShiftRight | ShiftRightLogical | LogicalAnd
  | LogicalOr | Eq | Lt | Gt | Le | Ge | ObjectMem | ListMem ->
    true
  | _ -> false

let label_of_const (c : const) : string =
  match c with
  | MAX_VALUE -> "Const.MAX_VALUE"
  | MIN_VALUE -> "Const.MIN_VALUE"
  | PI -> "Const.PI"

let label_of_unopt (op : unopt) : string =
  match op with
  | Typeof -> "typeof"
  | Neg -> "Arith.neg (-)"
  | BitwiseNot -> "Bitwise.not (~)"
  | LogicalNot -> "Logical.not (!)"
  | IntToFloat -> "Integer.int_to_float"
  | IntToString -> "Integer.int_to_string"
  | IntToFourHex -> "Integer.int_to_four_hex"
  | OctalToDecimal -> "Integer.octal_to_decimal"
  | FloatToInt -> "Float.float_to_int"
  | FloatToString -> "Float.float_to_string"
  | ToInt -> "Float.to_int"
  | ToInt32 -> "Float.to_int32"
  | ToUint16 -> "Float.to_uint16"
  | ToUint32 -> "Float.to_uint32"
  | IsNaN -> "Float.is_NaN"
  | StringToInt -> "String.string_to_int"
  | StringToFloat -> "String.string_to_float"
  | FromCharCode -> "String.from_char_code"
  | FromCharCodeU -> "String.from_char_code_u"
  | ToCharCode -> "String.to_char_code_u"
  | ToCharCodeU -> "String.to_char_code_u"
  | ToLowerCase -> "String.to_lower_case"
  | ToUpperCase -> "String.to_upper_case"
  | Trim -> "String.trim"
  | StringLen -> "String.s_len"
  | StringLenU -> "String.s_len_u"
  | StringConcat -> "String.s_concat"
  | ObjectToList -> "Object.obj_to_list"
  | ObjectFields -> "Object.obj_fields"
  | ArrayLen -> "Array.a_len"
  | ListToArray -> "List.list_to_array"
  | ListHead -> "List.hd"
  | ListTail -> "List.tl"
  | ListLen -> "List.l_len"
  | ListSort -> "List.l_sort"
  | ListReverse -> "List.l_reverse"
  | ListRemoveLast -> "List.l_remove"
  | TupleFirst -> "Tuple.fst"
  | TupleSecond -> "Tuple.snd"
  | TupleLen -> "Tup.t_len"
  | FloatToByte -> "Byte.float_to_byte"
  | Float32ToLEBytes -> "Byte.float32_to_le_bytes"
  | Float32ToBEBytes -> "Byte.float32_to_be_bytes"
  | Float64ToLEBytes -> "Byte.float64_to_le_bytes"
  | Float64ToBEBytes -> "Byte.float64_to_be_bytes"
  | Float32FromLEBytes -> "Byte.float32_from_le_bytes"
  | Float32FromBEBytes -> "Byte.float32_from_be_bytes"
  | Float64FromLEBytes -> "Byte.float64_from_le_bytes"
  | Float64FromBEBytes -> "Byte.float64_from_be_bytes"
  | BytesToString -> "Byte.bytes_to_string"
  | Random -> "Math.random"
  | Abs -> "Math.abs"
  | Sqrt -> "Math.sqrt"
  | Ceil -> "Math.ceil"
  | Floor -> "Math.floor"
  | Exp -> "Math.exp"
  | Log2 -> "Math.log_2"
  | LogE -> "Math.log_e"
  | Log10 -> "Math.log_10"
  | Sin -> "Math.sin"
  | Cos -> "Math.cos"
  | Tan -> "Math.tan"
  | Sinh -> "Math.sinh"
  | Cosh -> "Math.cosh"
  | Tanh -> "Math.tanh"
  | Acos -> "Math.acos"
  | Asin -> "Math.asin"
  | Atan -> "Math.atan"
  | Utf8Decode -> "Parse.utf8_decode"
  | HexDecode -> "Parse.hex_decode"
  | ParseNumber -> "Parse.parse_number"
  | ParseString -> "Parse.parse_string"
  | ParseDate -> "Parse.parse_date"

let label_of_binopt (op : binopt) : string =
  match op with
  | Plus -> "Arith.plus (+)"
  | Minus -> "Arith.minus (-)"
  | Times -> "Arith.times (*)"
  | Div -> "Arith.div (/)"
  | Modulo -> "Arith.mod (%)"
  | Pow -> "Arith.pow (**)"
  | BitwiseAnd -> "Bitwise.and (&)"
  | BitwiseOr -> "Bitwise.or (|)"
  | BitwiseXor -> "Bitwise.xor (^)"
  | ShiftLeft -> "Bitwise.shift_left (<<)"
  | ShiftRight -> "Bitwise.shift_right (>>)"
  | ShiftRightLogical -> "Bitwise.shift_right_logical (>>>)"
  | LogicalAnd -> "Logical.and (&&)"
  | LogicalOr -> "Logical.or (||)"
  | Eq -> "Comp.eq (=)"
  | Lt -> "Comp.lt (<)"
  | Gt -> "Comp.gt (>)"
  | Le -> "Comp.le (<=)"
  | Ge -> "Comp.ge (>=)"
  | ToPrecision -> "Float.to_precision"
  | ToExponential -> "Float.to_exponential"
  | ToFixed -> "Float.to_fixed"
  | ObjectMem -> "Object.in_obj"
  | StringNth -> "String.s_nth"
  | StringNthU -> "String.s_nth_u"
  | StringSplit -> "String.s_split"
  | ArrayMake -> "Array.array_make"
  | ArrayNth -> "Array.a_nth"
  | ListMem -> "List.in_list"
  | ListNth -> "List.l_nth"
  | ListAdd -> "List.l_add"
  | ListPrepend -> "List.l_prepend"
  | ListConcat -> "List.l_concat"
  | ListRemove -> "List.l_remove"
  | ListRemoveNth -> "List.l_remove_nth"
  | TupleNth -> "Tuple.t_nth"
  | IntToBEBytes -> "Byte.int_to_be_bytes"
  | IntFromLEBytes -> "Byte.int_from_le_bytes"
  | UintFromLEBytes -> "Byte.uint_from_le_bytes"
  | Min -> "Math.min"
  | Max -> "Math.max"
  | Atan2 -> "Math.atan2"

let label_of_triopt (op : triopt) : string =
  match op with
  | ITE -> "IfThenElse"
  | StringSubstr -> "String.s_substr"
  | StringSubstrU -> "String.s_substr_u"
  | ArraySet -> "Array.a_set"
  | ListSet -> "List.l_set"

let label_of_nopt (op : nopt) : string =
  match op with
  | NAryLogicalAnd -> "Logical.nary_and"
  | NAryLogicalOr -> "Logical.nary_or"
  | ArrayExpr -> "Array.a_expr"
  | ListExpr -> "List.l_expr"
  | TupleExpr -> "Tuple.t_expr"

let pp_of_unopt_single (fmt : Format.formatter) (op : unopt) : unit =
  let open Format in
  match op with
  | Typeof -> fprintf fmt "typeof"
  | Neg -> fprintf fmt "-"
  | BitwiseNot -> fprintf fmt "~"
  | LogicalNot -> fprintf fmt "!"
  | IntToFloat -> fprintf fmt "int_to_float"
  | IntToString -> fprintf fmt "int_to_string"
  | IntToFourHex -> fprintf fmt "int_to_four_hex"
  | OctalToDecimal -> fprintf fmt "octal_to_decimal"
  | FloatToInt -> fprintf fmt "int_of_float"
  | FloatToString -> fprintf fmt "float_to_string"
  | ToInt -> fprintf fmt "to_int"
  | ToInt32 -> fprintf fmt "to_int32"
  | ToUint16 -> fprintf fmt "to_uint16"
  | ToUint32 -> fprintf fmt "to_uint32"
  | IsNaN -> fprintf fmt "is_NaN"
  | StringToInt -> fprintf fmt "int_of_string"
  | StringToFloat -> fprintf fmt "float_of_string"
  | FromCharCode -> fprintf fmt "from_char_code"
  | FromCharCodeU -> fprintf fmt "from_char_code_u"
  | ToCharCode -> fprintf fmt "to_char_code"
  | ToCharCodeU -> fprintf fmt "to_char_code_u"
  | ToLowerCase -> fprintf fmt "to_lower_case"
  | ToUpperCase -> fprintf fmt "to_upper_case"
  | Trim -> fprintf fmt "trim"
  | StringLen -> fprintf fmt "s_len"
  | StringLenU -> fprintf fmt "s_len_u"
  | StringConcat -> fprintf fmt "s_concat"
  | ObjectToList -> fprintf fmt "obj_to_list"
  | ObjectFields -> fprintf fmt "obj_fields"
  | ArrayLen -> fprintf fmt "a_len"
  | ListToArray -> fprintf fmt "list_to_array"
  | ListHead -> fprintf fmt "hd"
  | ListTail -> fprintf fmt "tl"
  | ListLen -> fprintf fmt "l_len"
  | ListSort -> fprintf fmt "l_sort"
  | ListReverse -> fprintf fmt "l_reverse"
  | ListRemoveLast -> fprintf fmt "l_remove_last"
  | TupleFirst -> fprintf fmt "fst"
  | TupleSecond -> fprintf fmt "snd"
  | TupleLen -> fprintf fmt "t_len"
  | FloatToByte -> fprintf fmt "float_to_byte"
  | Float32ToLEBytes -> fprintf fmt "float32_to_le_bytes"
  | Float32ToBEBytes -> fprintf fmt "float32_to_be_bytes"
  | Float64ToLEBytes -> fprintf fmt "float64_to_le_bytes"
  | Float64ToBEBytes -> fprintf fmt "float64_to_be_bytes"
  | Float32FromLEBytes -> fprintf fmt "float32_from_le_bytes"
  | Float32FromBEBytes -> fprintf fmt "float32_from_be_bytes"
  | Float64FromLEBytes -> fprintf fmt "float64_from_le_bytes"
  | Float64FromBEBytes -> fprintf fmt "float64_from_be_bytes"
  | BytesToString -> fprintf fmt "bytes_to_string"
  | Random -> fprintf fmt "random"
  | Abs -> fprintf fmt "abs"
  | Sqrt -> fprintf fmt "sqrt"
  | Ceil -> fprintf fmt "ceil"
  | Floor -> fprintf fmt "floor"
  | Exp -> fprintf fmt "exp"
  | Log2 -> fprintf fmt "log_2"
  | LogE -> fprintf fmt "log_e"
  | Log10 -> fprintf fmt "log_10"
  | Sin -> fprintf fmt "sin"
  | Cos -> fprintf fmt "cos"
  | Tan -> fprintf fmt "tan"
  | Sinh -> fprintf fmt "sinh"
  | Cosh -> fprintf fmt "cosh"
  | Tanh -> fprintf fmt "tanh"
  | Asin -> fprintf fmt "asin"
  | Acos -> fprintf fmt "acos"
  | Atan -> fprintf fmt "atan"
  | Utf8Decode -> fprintf fmt "utf8_decode"
  | HexDecode -> fprintf fmt "hex_decode"
  | ParseNumber -> fprintf fmt "parse_number"
  | ParseString -> fprintf fmt "parse_string"
  | ParseDate -> fprintf fmt "parse_date"

let pp_of_binopt_single (fmt : Format.formatter) (op : binopt) : unit =
  let open Format in
  match op with
  | Plus -> fprintf fmt "+"
  | Minus -> fprintf fmt "-"
  | Times -> fprintf fmt "*"
  | Div -> fprintf fmt "/"
  | Modulo -> fprintf fmt "%%"
  | Pow -> fprintf fmt "**"
  | BitwiseAnd -> fprintf fmt "&"
  | BitwiseOr -> fprintf fmt "|"
  | BitwiseXor -> fprintf fmt "^"
  | ShiftLeft -> fprintf fmt "<<"
  | ShiftRight -> fprintf fmt ">>"
  | ShiftRightLogical -> fprintf fmt ">>>"
  | LogicalAnd -> fprintf fmt "&&"
  | LogicalOr -> fprintf fmt "||"
  | Eq -> fprintf fmt "="
  | Lt -> fprintf fmt "<"
  | Gt -> fprintf fmt ">"
  | Le -> fprintf fmt "<="
  | Ge -> fprintf fmt ">="
  | ToPrecision -> fprintf fmt "to_precision"
  | ToExponential -> fprintf fmt "to_exponential"
  | ToFixed -> fprintf fmt "to_fixed"
  | ObjectMem -> fprintf fmt "in_obj"
  | StringNth -> fprintf fmt "s_nth"
  | StringNthU -> fprintf fmt "s_nth_u"
  | StringSplit -> fprintf fmt "s_split"
  | ArrayMake -> fprintf fmt "array_make"
  | ArrayNth -> fprintf fmt "a_nth"
  | ListMem -> fprintf fmt "in_list"
  | ListNth -> fprintf fmt "l_nth"
  | ListAdd -> fprintf fmt "l_add"
  | ListPrepend -> fprintf fmt "l_prepend"
  | ListConcat -> fprintf fmt "l_concat"
  | ListRemove -> fprintf fmt "l_remove"
  | ListRemoveNth -> fprintf fmt "l_remove_nth"
  | TupleNth -> fprintf fmt "t_nth"
  | IntToBEBytes -> fprintf fmt "int_to_be_bytes"
  | IntFromLEBytes -> fprintf fmt "int_from_le_bytes"
  | UintFromLEBytes -> fprintf fmt "uint_from_le_bytes"
  | Min -> fprintf fmt "min"
  | Max -> fprintf fmt "max"
  | Atan2 -> fprintf fmt "atan2"

let pp_of_triopt_single (fmt : Format.formatter) (op : triopt) : unit =
  let open Format in
  match op with
  | ITE -> fprintf fmt "ite"
  | StringSubstr -> fprintf fmt "s_substr"
  | StringSubstrU -> fprintf fmt "s_substr_u"
  | ArraySet -> fprintf fmt "a_set"
  | ListSet -> fprintf fmt "l_set"

let pp_of_const (fmt : Format.formatter) (c : const) : unit =
  let open Format in
  match c with
  | MAX_VALUE -> fprintf fmt "MAX_VALUE"
  | MIN_VALUE -> fprintf fmt "MIN_VALUE"
  | PI -> fprintf fmt "PI"

let pp_of_unopt (pp_val : 'a pp_fmt) (fmt : Format.formatter)
  ((op, v) : unopt * 'a) : unit =
  if is_infix_unopt op then
    Format.fprintf fmt "%a%a" pp_of_unopt_single op pp_val v
  else Format.fprintf fmt "%a(%a)" pp_of_unopt_single op pp_val v

let pp_of_binopt (pp_val : 'a pp_fmt) (fmt : Format.formatter)
  ((op, v1, v2) : binopt * 'a * 'a) : unit =
  if is_infix_binopt op then
    Format.fprintf fmt "%a %a %a" pp_val v1 pp_of_binopt_single op pp_val v2
  else
    Format.fprintf fmt "%a(%a, %a)" pp_of_binopt_single op pp_val v1 pp_val v2

let pp_of_triopt (pp_val : 'a pp_fmt) (fmt : Format.formatter)
  ((op, v1, v2, v3) : triopt * 'a * 'a * 'a) : unit =
  Format.fprintf fmt "%a(%a, %a, %a)" pp_of_triopt_single op pp_val v1 pp_val v2
    pp_val v3

let pp_of_nopt (pp_val : 'a pp_fmt) (fmt : Format.formatter)
  ((op, vs) : nopt * 'a list) : unit =
  let open Format in
  let pp_sep sep fmt () = pp_print_string fmt sep in
  let pp_lst sep pp fmt lst = pp_print_list ~pp_sep:(pp_sep sep) pp fmt lst in
  match (op, vs) with
  | (NAryLogicalAnd, _) -> fprintf fmt "%a" (pp_lst " && " pp_val) vs
  | (NAryLogicalOr, _) -> fprintf fmt "%a" (pp_lst " || " pp_val) vs
  | (ArrayExpr, []) -> fprintf fmt "[| |]"
  | (ArrayExpr, _) -> fprintf fmt "[|%a|]" (pp_lst ", " pp_val) vs
  | (ListExpr, []) -> fprintf fmt "[]"
  | (ListExpr, _) -> fprintf fmt "[ %a ]" (pp_lst ", " pp_val) vs
  | (TupleExpr, []) -> fprintf fmt "()"
  | (TupleExpr, _) -> fprintf fmt "( %a )" (pp_lst ", " pp_val) vs

let str_of_unopt_single (op : unopt) : string =
  Format.asprintf "%a" pp_of_unopt_single op

let str_of_binopt_single (op : binopt) : string =
  Format.asprintf "%a" pp_of_binopt_single op

let str_of_triopt_single (op : triopt) : string =
  Format.asprintf "%a" pp_of_triopt_single op

let str_of_const (c : const) : string = Format.asprintf "%a" pp_of_const c

let str_of_unopt (pp_val : 'a pp_fmt) (op : unopt) (v : 'a) : string =
  Format.asprintf "%a" (pp_of_unopt pp_val) (op, v)

let str_of_binopt (pp_val : 'a pp_fmt) (op : binopt) (v1 : 'a) (v2 : 'a) :
  string =
  Format.asprintf "%a" (pp_of_binopt pp_val) (op, v1, v2)

let str_of_triopt (pp_val : 'a pp_fmt) (op : triopt) (v1 : 'a) (v2 : 'a)
  (v3 : 'a) : string =
  Format.asprintf "%a" (pp_of_triopt pp_val) (op, v1, v2, v3)

let str_of_nopt (pp_val : 'a pp_fmt) (op : nopt) (vs : 'a list) : string =
  Format.asprintf "%a" (pp_of_nopt pp_val) (op, vs)
