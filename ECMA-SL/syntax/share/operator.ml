open EslBase

type unopt =
  | Neg
  | BitwiseNot
  | LogicalNot
  | ListHead
  | ListTail
  | Typeof
  | IntToFloat
  | IntToString
  | FloatToInt
  | FloatToString
  | StringToInt
  | StringToFloat
  | ObjectToList
  | ObjectFields

type binopt =
  | Plus
  | Minus
  | Times
  | Div
  | Modulo
  | Pow
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | ShiftLeft
  | ShiftRight
  | ShiftRightLogical
  | LogicalAnd
  | LogicalOr
  | SCLogicalAnd
  | SCLogicalOr
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge
  | ObjectMem

type triopt = Conditional

type nopt =
  | ListExpr
  | NAryLogicalAnd
  | NAryLogicalOr

let unopt_label (op : unopt) : string =
  match op with
  | Neg -> "Arith.neg (-)"
  | BitwiseNot -> "Bitwise.not (~)"
  | LogicalNot -> "Logical.not (!)"
  | ListHead -> "List.head (hd)"
  | ListTail -> "List.tail (tl)"
  | Typeof -> "Type.of (typeof)"
  | IntToFloat -> "Type.intToFloat (int_to_float)"
  | IntToString -> "Type.intToString (int_to_string)"
  | FloatToInt -> "Type.floatToInt (float_to_int)"
  | FloatToString -> "Type.floatToString (float_to_string)"
  | StringToInt -> "Type.stringToInt (string_to_int)"
  | StringToFloat -> "Type.stringToFloat (string_to_float)"
  | ObjectToList -> "Object.toList (obj_to_list)"
  | ObjectFields -> "Object.fields (obj_fields)"

let binopt_label (op : binopt) : string =
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
  | ShiftLeft -> "Bitwise.shl (<<)"
  | ShiftRight -> "Bitwise.shrA (>>)"
  | ShiftRightLogical -> "Bitwise.shrL (>>>)"
  | LogicalAnd -> "Logical.and (&&)"
  | LogicalOr -> "Logical.or (||)"
  | SCLogicalAnd -> "Logical.scAnd (&&&)"
  | SCLogicalOr -> "Logical.scOr (|||)"
  | Eq -> "Relation.eq (==)"
  | Ne -> "Relation.ne (!=)"
  | Lt -> "Relation.lt (<)"
  | Gt -> "Relation.gt (>)"
  | Le -> "Relation.le (<=)"
  | Ge -> "Relation.ge (>=)"
  | ObjectMem -> "Object.in (in_obj)"

let triopt_label (op : triopt) : string =
  match op with Conditional -> "Relation.conditional (?)"

let nopt_label (op : nopt) : string =
  match op with
  | ListExpr -> "List.expr ([...])"
  | NAryLogicalAnd -> "Logical.nAnd (&&...)"
  | NAryLogicalOr -> "Logical.nOr (||...)"

let unopt_pp_simple (ppf : Fmt.t) (op : unopt) : unit =
  match op with
  | Neg -> Fmt.pp_str ppf "-"
  | BitwiseNot -> Fmt.pp_str ppf "~"
  | LogicalNot -> Fmt.pp_str ppf "!"
  | ListHead -> Fmt.pp_str ppf "hd"
  | ListTail -> Fmt.pp_str ppf "tl"
  | Typeof -> Fmt.pp_str ppf "typeof"
  | IntToFloat -> Fmt.pp_str ppf "int_to_float"
  | IntToString -> Fmt.pp_str ppf "int_to_string"
  | FloatToInt -> Fmt.pp_str ppf "float_to_int"
  | FloatToString -> Fmt.pp_str ppf "float_to_string"
  | StringToInt -> Fmt.pp_str ppf "string_to_int"
  | StringToFloat -> Fmt.pp_str ppf "string_to_float"
  | ObjectToList -> Fmt.pp_str ppf "obj_to_list"
  | ObjectFields -> Fmt.pp_str ppf "obj_fields"

let binopt_pp_simple (ppf : Fmt.t) (op : binopt) : unit =
  match op with
  | Plus -> Fmt.pp_str ppf "+"
  | Minus -> Fmt.pp_str ppf "-"
  | Times -> Fmt.pp_str ppf "*"
  | Div -> Fmt.pp_str ppf "/"
  | Modulo -> Fmt.pp_str ppf "%%"
  | Pow -> Fmt.pp_str ppf "**"
  | BitwiseAnd -> Fmt.pp_str ppf "&"
  | BitwiseOr -> Fmt.pp_str ppf "|"
  | BitwiseXor -> Fmt.pp_str ppf "^"
  | ShiftLeft -> Fmt.pp_str ppf "<<"
  | ShiftRight -> Fmt.pp_str ppf ">>"
  | ShiftRightLogical -> Fmt.pp_str ppf ">>>"
  | LogicalAnd -> Fmt.pp_str ppf "&&"
  | LogicalOr -> Fmt.pp_str ppf "||"
  | SCLogicalAnd -> Fmt.pp_str ppf "&&&"
  | SCLogicalOr -> Fmt.pp_str ppf "|||"
  | Eq -> Fmt.pp_str ppf "=="
  | Ne -> Fmt.pp_str ppf "!="
  | Lt -> Fmt.pp_str ppf "<"
  | Gt -> Fmt.pp_str ppf ">"
  | Le -> Fmt.pp_str ppf "<="
  | Ge -> Fmt.pp_str ppf ">="
  | ObjectMem -> Fmt.pp_str ppf "in_obj"

let triopt_pp_simple (ppf : Fmt.t) (op : triopt) : unit =
  match op with Conditional -> Fmt.pp_str ppf "?"

let nopt_pp_simple (ppf : Fmt.t) (op : nopt) : unit =
  match op with
  | ListExpr -> Fmt.pp_str ppf "[...]"
  | NAryLogicalAnd -> Fmt.pp_str ppf "&&..."
  | NAryLogicalOr -> Fmt.pp_str ppf "||..."

let unopt_pp ~(pp_v : Fmt.t -> 'a -> unit) (ppf : Fmt.t) ((op, v) : unopt * 'a)
  : unit =
  match op with
  | Neg -> Fmt.fmt ppf "%a%a" unopt_pp_simple op pp_v v
  | BitwiseNot -> Fmt.fmt ppf "%a%a" unopt_pp_simple op pp_v v
  | LogicalNot -> Fmt.fmt ppf "%a%a" unopt_pp_simple op pp_v v
  | _ -> Fmt.fmt ppf "%a %a" unopt_pp_simple op pp_v v

let binopt_pp ~(pp_v : Fmt.t -> 'a -> unit) (ppf : Fmt.t)
  ((op, v1, v2) : binopt * 'a * 'a) : unit =
  Fmt.fmt ppf "%a %a %a" pp_v v1 binopt_pp_simple op pp_v v2

let triopt_pp ~(pp_v : Fmt.t -> 'a -> unit) (ppf : Fmt.t)
  ((op, v1, v2, v3) : triopt * 'a * 'a * 'a) : unit =
  match op with
  | Conditional -> Fmt.fmt ppf "%a ? %a : %a" pp_v v1 pp_v v2 pp_v v3

let nopt_pp ~(pp_v : Fmt.t -> 'a -> unit) (ppf : Fmt.t)
  ((op, vs) : nopt * 'a list) : unit =
  match op with
  | ListExpr -> Fmt.fmt ppf "[%a]" Fmt.(pp_lst !>", " pp_v) vs
  | NAryLogicalAnd -> Fmt.fmt ppf "%a" Fmt.(pp_lst !>" && " pp_v) vs
  | NAryLogicalOr -> Fmt.fmt ppf "%a" Fmt.(pp_lst !>" || " pp_v) vs

let unopt_str_simple (op : unopt) : string = Fmt.str "%a" unopt_pp_simple op
[@@inline]

let binopt_str_simple (op : binopt) : string = Fmt.str "%a" binopt_pp_simple op
[@@inline]

let triopt_str_simple (op : triopt) : string = Fmt.str "%a" triopt_pp_simple op
[@@inline]

let nopt_str_simple (op : nopt) : string = Fmt.str "%a" nopt_pp_simple op
[@@inline]

let unopt_str ~(pp_v : Fmt.t -> 'a -> unit) ((op, v) : unopt * 'a) : string =
  Fmt.str "%a" (unopt_pp ~pp_v) (op, v)
[@@inline]

let binopt_str ~(pp_v : Fmt.t -> 'a -> unit) ((op, v1, v2) : binopt * 'a * 'a) :
  string =
  Fmt.str "%a" (binopt_pp ~pp_v) (op, v1, v2)
[@@inline]

let triopt_str ~(pp_v : Fmt.t -> 'a -> unit)
  ((op, v1, v2, v3) : triopt * 'a * 'a * 'a) : string =
  Fmt.str "%a" (triopt_pp ~pp_v) (op, v1, v2, v3)
[@@inline]

let nopt_str ~(pp_v : Fmt.t -> 'a -> unit) ((op, vs) : nopt * 'a list) : string
    =
  Fmt.str "%a" (nopt_pp ~pp_v) (op, vs)
[@@inline]
