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

let str_of_const (c : const) : string =
  match c with
  | MAX_VALUE -> "MAX_VALUE"
  | MIN_VALUE -> "MIN_VALUE"
  | PI -> "PI"

let str_of_unopt_single (op : unopt) : string =
  match op with
  | Typeof -> "typeof"
  | Neg -> "-"
  | BitwiseNot -> "~"
  | LogicalNot -> "!"
  | IntToFloat -> "int_to_float"
  | IntToString -> "int_to_string"
  | IntToFourHex -> "int_to_four_hex"
  | OctalToDecimal -> "octal_to_decimal"
  | FloatToInt -> "int_of_float"
  | FloatToString -> "float_to_string"
  | ToInt -> "to_int"
  | ToInt32 -> "to_int32"
  | ToUint16 -> "to_uint16"
  | ToUint32 -> "to_uint32"
  | IsNaN -> "is_NaN"
  | StringToInt -> "int_of_string"
  | StringToFloat -> "float_of_string"
  | FromCharCode -> "from_char_code"
  | FromCharCodeU -> "from_char_code_u"
  | ToCharCode -> "to_char_code"
  | ToCharCodeU -> "to_char_code_u"
  | ToLowerCase -> "to_lower_case"
  | ToUpperCase -> "to_upper_case"
  | Trim -> "trim"
  | StringLen -> "s_len"
  | StringLenU -> "s_len_u"
  | StringConcat -> "s_concat"
  | ObjectToList -> "obj_to_list"
  | ObjectFields -> "obj_fields"
  | ArrayLen -> "a_len"
  | ListToArray -> "list_to_array"
  | ListHead -> "hd"
  | ListTail -> "tl"
  | ListLen -> "l_len"
  | ListSort -> "l_sort"
  | ListReverse -> "l_reverse"
  | ListRemoveLast -> "l_remove_last"
  | TupleFirst -> "fst"
  | TupleSecond -> "snd"
  | TupleLen -> "t_len"
  | FloatToByte -> "float_to_byte"
  | Float32ToLEBytes -> "float32_to_le_bytes"
  | Float32ToBEBytes -> "float32_to_be_bytes"
  | Float64ToLEBytes -> "float64_to_le_bytes"
  | Float64ToBEBytes -> "float64_to_be_bytes"
  | Float32FromLEBytes -> "float32_from_le_bytes"
  | Float32FromBEBytes -> "float32_from_be_bytes"
  | Float64FromLEBytes -> "float64_from_le_bytes"
  | Float64FromBEBytes -> "float64_from_be_bytes"
  | BytesToString -> "bytes_to_string"
  | Random -> "random"
  | Abs -> "abs"
  | Sqrt -> "sqrt"
  | Ceil -> "ceil"
  | Floor -> "floor"
  | Exp -> "exp"
  | Log2 -> "log_2"
  | LogE -> "log_e"
  | Log10 -> "log_10"
  | Sin -> "sin"
  | Cos -> "cos"
  | Tan -> "tan"
  | Sinh -> "sinh"
  | Cosh -> "cosh"
  | Tanh -> "tanh"
  | Asin -> "asin"
  | Acos -> "acos"
  | Atan -> "atan"
  | Utf8Decode -> "utf8_decode"
  | HexDecode -> "hex_decode"
  | ParseNumber -> "parse_number"
  | ParseString -> "parse_string"
  | ParseDate -> "parse_date"

let str_of_unopt (op : unopt) (v : string) : string =
  match op with
  | Typeof -> Printf.sprintf "typeof(%s)" v
  | Neg -> Printf.sprintf "-%s" v
  | BitwiseNot -> Printf.sprintf "~%s" v
  | LogicalNot -> Printf.sprintf "!%s" v
  | IntToFloat -> Printf.sprintf "int_to_float(%s)" v
  | IntToString -> Printf.sprintf "int_to_string(%s)" v
  | IntToFourHex -> Printf.sprintf "int_to_four_hex(%s)" v
  | OctalToDecimal -> Printf.sprintf "octal_to_decimal(%s)" v
  | FloatToInt -> Printf.sprintf "int_of_float(%s)" v
  | FloatToString -> Printf.sprintf "float_to_string(%s)" v
  | ToInt -> Printf.sprintf "to_int(%s)" v
  | ToInt32 -> Printf.sprintf "to_int32(%s)" v
  | ToUint16 -> Printf.sprintf "to_uint16(%s)" v
  | ToUint32 -> Printf.sprintf "to_uint32(%s)" v
  | IsNaN -> Printf.sprintf "is_NaN(%s)" v
  | StringToInt -> Printf.sprintf "int_of_string(%s)" v
  | StringToFloat -> Printf.sprintf "float_of_string(%s)" v
  | FromCharCode -> Printf.sprintf "from_char_code(%s)" v
  | FromCharCodeU -> Printf.sprintf "from_char_code_u(%s)" v
  | ToCharCode -> Printf.sprintf "to_char_code(%s)" v
  | ToCharCodeU -> Printf.sprintf "to_char_code_u(%s)" v
  | ToLowerCase -> Printf.sprintf "to_lower_case(%s)" v
  | ToUpperCase -> Printf.sprintf "to_upper_case(%s)" v
  | Trim -> Printf.sprintf "trim(%s)" v
  | StringLen -> Printf.sprintf "s_len(%s)" v
  | StringLenU -> Printf.sprintf "s_len_u(%s)" v
  | StringConcat -> Printf.sprintf "s_concat(%s)" v
  | ObjectToList -> Printf.sprintf "obj_to_list(%s)" v
  | ObjectFields -> Printf.sprintf "obj_fields(%s)" v
  | ArrayLen -> Printf.sprintf "a_len(%s)" v
  | ListToArray -> Printf.sprintf "list_to_array(%s)" v
  | ListHead -> Printf.sprintf "hd(%s)" v
  | ListTail -> Printf.sprintf "tl(%s)" v
  | ListLen -> Printf.sprintf "l_len(%s)" v
  | ListSort -> Printf.sprintf "l_sort(%s)" v
  | ListReverse -> Printf.sprintf "l_reverse(%s)" v
  | ListRemoveLast -> Printf.sprintf "l_remove_last(%s)" v
  | TupleFirst -> Printf.sprintf "fst(%s)" v
  | TupleSecond -> Printf.sprintf "snd(%s)" v
  | TupleLen -> Printf.sprintf "t_len(%s)" v
  | FloatToByte -> Printf.sprintf "float_to_byte(%s)" v
  | Float32ToLEBytes -> Printf.sprintf "float32_to_le_bytes(%s)" v
  | Float32ToBEBytes -> Printf.sprintf "float32_to_be_bytes(%s)" v
  | Float64ToLEBytes -> Printf.sprintf "float64_to_le_bytes(%s)" v
  | Float64ToBEBytes -> Printf.sprintf "float64_to_be_bytes(%s)" v
  | Float32FromLEBytes -> Printf.sprintf "float32_from_le_bytes(%s)" v
  | Float32FromBEBytes -> Printf.sprintf "float32_from_be_bytes(%s)" v
  | Float64FromLEBytes -> Printf.sprintf "float64_from_le_bytes(%s)" v
  | Float64FromBEBytes -> Printf.sprintf "float64_from_be_bytes(%s)" v
  | BytesToString -> Printf.sprintf "bytes_to_string(%s)" v
  | Random -> Printf.sprintf "random(%s)" v
  | Abs -> Printf.sprintf "abs(%s)" v
  | Sqrt -> Printf.sprintf "sqrt(%s)" v
  | Ceil -> Printf.sprintf "ceil(%s)" v
  | Floor -> Printf.sprintf "floor(%s)" v
  | Exp -> Printf.sprintf "exp(%s)" v
  | Log2 -> Printf.sprintf "log_2(%s)" v
  | LogE -> Printf.sprintf "log_e(%s)" v
  | Log10 -> Printf.sprintf "log_10(%s)" v
  | Sin -> Printf.sprintf "sin(%s)" v
  | Cos -> Printf.sprintf "cos(%s)" v
  | Tan -> Printf.sprintf "tan(%s)" v
  | Sinh -> Printf.sprintf "sinh(%s)" v
  | Cosh -> Printf.sprintf "cosh(%s)" v
  | Tanh -> Printf.sprintf "tanh(%s)" v
  | Asin -> Printf.sprintf "asin(%s)" v
  | Acos -> Printf.sprintf "acos(%s)" v
  | Atan -> Printf.sprintf "atan(%s)" v
  | Utf8Decode -> Printf.sprintf "utf8_decode(%s)" v
  | HexDecode -> Printf.sprintf "hex_decode(%s)" v
  | ParseNumber -> Printf.sprintf "parse_number(%s)" v
  | ParseString -> Printf.sprintf "parse_string(%s)" v
  | ParseDate -> Printf.sprintf "parse_date(%s)" v

let str_of_binopt_single (op : binopt) : string =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Modulo -> "%"
  | Pow -> "**"
  | BitwiseAnd -> "&"
  | BitwiseOr -> "|"
  | BitwiseXor -> "^"
  | ShiftLeft -> "<<"
  | ShiftRight -> ">>"
  | ShiftRightLogical -> ">>>"
  | LogicalAnd -> "&&"
  | LogicalOr -> "||"
  | Eq -> "="
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | ToPrecision -> "to_precision"
  | ToExponential -> "to_exponential"
  | ToFixed -> "to_fixed"
  | ObjectMem -> "in_obj"
  | StringNth -> "s_nth"
  | StringNthU -> "s_nth_u"
  | StringSplit -> "s_split"
  | ArrayMake -> "array_make"
  | ArrayNth -> "a_nth"
  | ListMem -> "in_list"
  | ListNth -> "l_nth"
  | ListAdd -> "l_add"
  | ListPrepend -> "l_prepend"
  | ListConcat -> "l_concat"
  | ListRemove -> "l_remove"
  | ListRemoveNth -> "l_remove_nth"
  | TupleNth -> "t_nth"
  | IntToBEBytes -> "int_to_be_bytes"
  | IntFromLEBytes -> "int_from_le_bytes"
  | UintFromLEBytes -> "uint_from_le_bytes"
  | Min -> "min"
  | Max -> "max"
  | Atan2 -> "atan2"

let str_of_binopt (op : binopt) (v1 : string) (v2 : string) : string =
  match op with
  | Plus -> Printf.sprintf "%s + %s" v1 v2
  | Minus -> Printf.sprintf "%s - %s" v1 v2
  | Times -> Printf.sprintf "%s * %s" v1 v2
  | Div -> Printf.sprintf "%s / %s" v1 v2
  | Modulo -> Printf.sprintf "%s %% %s" v1 v2
  | Pow -> Printf.sprintf "%s ** %s" v1 v2
  | BitwiseAnd -> Printf.sprintf "%s & %s" v1 v2
  | BitwiseOr -> Printf.sprintf "%s | %s" v1 v2
  | BitwiseXor -> Printf.sprintf "%s ^ %s" v1 v2
  | ShiftLeft -> Printf.sprintf "%s << %s" v1 v2
  | ShiftRight -> Printf.sprintf "%s >> %s" v1 v2
  | ShiftRightLogical -> Printf.sprintf "%s >>> %s" v1 v2
  | LogicalAnd -> Printf.sprintf "%s && %s" v1 v2
  | LogicalOr -> Printf.sprintf "%s || %s" v1 v2
  | Eq -> Printf.sprintf "%s = %s" v1 v2
  | Lt -> Printf.sprintf "%s < %s" v1 v2
  | Gt -> Printf.sprintf "%s > %s" v1 v2
  | Le -> Printf.sprintf "%s <= %s" v1 v2
  | Ge -> Printf.sprintf "%s >= %s" v1 v2
  | ToPrecision -> Printf.sprintf "to_precision(%s, %s)" v1 v2
  | ToExponential -> Printf.sprintf "to_exponential(%s, %s)" v1 v2
  | ToFixed -> Printf.sprintf "to_fixed(%s, %s)" v1 v2
  | ObjectMem -> Printf.sprintf "%s in_obj %s" v1 v2
  | StringNth -> Printf.sprintf "s_nth(%s, %s)" v1 v2
  | StringNthU -> Printf.sprintf "s_nth_u(%s, %s)" v1 v2
  | StringSplit -> Printf.sprintf "s_split(%s, %s)" v1 v2
  | ArrayMake -> Printf.sprintf "array_make(%s, %s)" v1 v2
  | ArrayNth -> Printf.sprintf "a_nth(%s, %s)" v1 v2
  | ListMem -> Printf.sprintf "%s in_list %s" v1 v2
  | ListNth -> Printf.sprintf "l_nth(%s, %s)" v1 v2
  | ListAdd -> Printf.sprintf "l_add(%s, %s)" v1 v2
  | ListPrepend -> Printf.sprintf "l_prepend(%s, %s)" v1 v2
  | ListConcat -> Printf.sprintf "l_concat(%s, %s)" v1 v2
  | ListRemove -> Printf.sprintf "l_remove(%s, %s)" v1 v2
  | ListRemoveNth -> Printf.sprintf "l_remove_nth(%s, %s)" v1 v2
  | TupleNth -> Printf.sprintf "t_nth(%s, %s)" v1 v2
  | IntToBEBytes -> Printf.sprintf "int_to_be_bytes(%s, %s)" v1 v2
  | IntFromLEBytes -> Printf.sprintf "int_from_le_bytes(%s, %s)" v1 v2
  | UintFromLEBytes -> Printf.sprintf "uint_from_le_bytes(%s, %s)" v1 v2
  | Min -> Printf.sprintf "min(%s, %s)" v1 v2
  | Max -> Printf.sprintf "max(%s, %s)" v1 v2
  | Atan2 -> Printf.sprintf "atan2(%s, %s)" v1 v2

let str_of_triopt_single (op : triopt) : string =
  match op with
  | ITE -> "ite"
  | StringSubstr -> "s_substr"
  | StringSubstrU -> "s_substr_u"
  | ArraySet -> "a_set"
  | ListSet -> "l_set"

let str_of_triopt (op : triopt) (v1 : string) (v2 : string) (v3 : string) :
  string =
  match op with
  | ITE -> Printf.sprintf "ite(%s, %s, %s)" v1 v2 v3
  | StringSubstr -> Printf.sprintf "s_substr(%s, %s, %s)" v1 v2 v3
  | StringSubstrU -> Printf.sprintf "s_substr_u(%s, %s, %s)" v1 v2 v3
  | ArraySet -> Printf.sprintf "a_set(%s, %s, %s)" v1 v2 v3
  | ListSet -> Printf.sprintf "l_set(%s, %s, %s)" v1 v2 v3

let str_of_nopt (op : nopt) (es : string list) : string =
  match op with
  | NAryLogicalAnd -> String.concat " && " es
  | NAryLogicalOr -> String.concat " || " es
  | ArrayExpr -> "[| " ^ String.concat ", " es ^ " |]"
  | ListExpr -> "[ " ^ String.concat ", " es ^ " ]"
  | TupleExpr -> "( " ^ String.concat ", " es ^ " )"

let unopt_to_json (op : unopt) : string =
  Printf.sprintf "{ \"type\" : \"unopt\", \"value\" : \"%s\" }"
    ( match op with
    | Typeof -> "Typeof"
    | Neg -> "Neg"
    | BitwiseNot -> "BitwiseNot"
    | LogicalNot -> "LogicalNot"
    | IntToFloat -> "IntToFloat"
    | IntToString -> "IntToString"
    | IntToFourHex -> "IntToFourHex"
    | OctalToDecimal -> "OctalToDecimal"
    | FloatToInt -> "FloatToInt"
    | FloatToString -> "FloatToString"
    | ToInt -> "ToInt"
    | ToInt32 -> "ToInt32"
    | ToUint16 -> "ToUint16"
    | ToUint32 -> "ToUint32"
    | IsNaN -> "IsNaN"
    | StringToInt -> "StringToInt"
    | StringToFloat -> "StringToFloat"
    | FromCharCode -> "FromCharCode"
    | FromCharCodeU -> "FromCharCodeU"
    | ToCharCode -> "ToCharCode"
    | ToCharCodeU -> "ToCharCodeU"
    | ToLowerCase -> "ToLowerCase"
    | ToUpperCase -> "ToUpperCase"
    | Trim -> "Trim"
    | StringLen -> "StringLen"
    | StringLenU -> "StringLenU"
    | StringConcat -> "StringConcat"
    | ObjectToList -> "ObjectToList"
    | ObjectFields -> "ObjectFields"
    | ArrayLen -> "ArrayLen"
    | ListToArray -> "ListToArray"
    | ListHead -> "ListHead"
    | ListTail -> "ListTail"
    | ListLen -> "ListLen"
    | ListSort -> "ListSort"
    | ListReverse -> "ListReverse"
    | ListRemoveLast -> "ListRemoveLast"
    | TupleFirst -> "TupleFirst"
    | TupleSecond -> "TupleSecond"
    | TupleLen -> "TupleLen"
    | FloatToByte -> "FloatToByte"
    | Float32ToLEBytes -> "Float32ToLEBytes"
    | Float32ToBEBytes -> "Float32ToBEBytes"
    | Float64ToLEBytes -> "Float64ToLEBytes"
    | Float64ToBEBytes -> "Float64ToBEBytes"
    | Float32FromLEBytes -> "Float32FromLEBytes"
    | Float32FromBEBytes -> "Float32FromBEBytes"
    | Float64FromLEBytes -> "Float64FromLEBytes"
    | Float64FromBEBytes -> "Float64FromBEBytes"
    | BytesToString -> "BytesToString"
    | Random -> "Random"
    | Abs -> "Abs"
    | Sqrt -> "Sqrt"
    | Ceil -> "Ceil"
    | Floor -> "Floor"
    | Exp -> "Exp"
    | Log2 -> "Log2"
    | LogE -> "LogE"
    | Log10 -> "Log10"
    | Sin -> "Sin"
    | Cos -> "Cos"
    | Tan -> "Tan"
    | Sinh -> "Sinh"
    | Cosh -> "Cosh"
    | Tanh -> "Tanh"
    | Asin -> "Asin"
    | Acos -> "Acos"
    | Atan -> "Atan"
    | Utf8Decode -> "Utf8Decode"
    | HexDecode -> "HexDecode"
    | ParseNumber -> "ParseNumber"
    | ParseString -> "ParseString"
    | ParseDate -> "ParseDate" )

let binopt_to_json (op : binopt) : string =
  Printf.sprintf "{ \"type\" : \"binopt\", \"value\" : \"%s\" }"
    ( match op with
    | Plus -> "Plus"
    | Minus -> "Minus"
    | Times -> "Times"
    | Div -> "Div"
    | Modulo -> "Modulo"
    | Pow -> "Pow"
    | BitwiseAnd -> "BitwiseAnd"
    | BitwiseOr -> "BitwiseOr"
    | BitwiseXor -> "BitwiseXor"
    | ShiftLeft -> "ShiftLeft"
    | ShiftRight -> "ShiftRight"
    | ShiftRightLogical -> "ShiftRightLogical"
    | LogicalAnd -> "LogicalAnd"
    | LogicalOr -> "LogicalOr"
    | Eq -> "Eq"
    | Lt -> "Lt"
    | Gt -> "Gt"
    | Le -> "Le"
    | Ge -> "Ge"
    | ToPrecision -> "ToPrecision"
    | ToExponential -> "ToExponential"
    | ToFixed -> "ToFixed"
    | ObjectMem -> "ObjectMem"
    | StringNth -> "StringNth"
    | StringNthU -> "StringNthU"
    | StringSplit -> "StringSplit"
    | ArrayMake -> "ArrayMake"
    | ArrayNth -> "ArrayNth"
    | ListMem -> "ListMem"
    | ListNth -> "ListNth"
    | ListAdd -> "ListAdd"
    | ListPrepend -> "ListPrepend"
    | ListConcat -> "ListConcat"
    | ListRemove -> "ListRemove"
    | ListRemoveNth -> "ListRemoveNth"
    | TupleNth -> "TupleNth"
    | IntToBEBytes -> "IntToBEBytes"
    | IntFromLEBytes -> "IntFromLEBytes"
    | UintFromLEBytes -> "UintFromLEBytes"
    | Min -> "Min"
    | Max -> "Max"
    | Atan2 -> "Atan2" )

let triopt_to_json (op : triopt) : string =
  Printf.sprintf "{ \"type\" : \"triopt\", \"value\" : \"%s\" }"
    ( match op with
    | ITE -> "ITE"
    | StringSubstr -> "StringSubstr"
    | StringSubstrU -> "StringSubstrU"
    | ArraySet -> "ArraySet"
    | ListSet -> "ListSet" )

let nopt_to_json (op : nopt) : string =
  Printf.sprintf "{ \"type\" : \"nopt\", \"value\" : \"%s\" }"
    ( match op with
    | NAryLogicalAnd -> "NAryLogicalAnd"
    | NAryLogicalOr -> "NAryLogicalOr"
    | ArrayExpr -> "ArrayExpr"
    | ListExpr -> "ListExpr"
    | TupleExpr -> "TupleExpr" )
