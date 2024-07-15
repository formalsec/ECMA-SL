open EslBase

type const =
  | MAX_VALUE
  | MIN_VALUE
  | PI

type unopt =
  (* Arithmetic operators *)
  | Neg
  (* Bitwise operators *)
  | BitwiseNot
  (* Logical *)
  | LogicalNot
  (* Integer operators *)
  | IntToFloat
  | IntToString
  (* Float operators *)
  | FloatToInt
  | FloatToString
  (* String operators *)
  | StringToInt
  | StringToFloat
  | FromCharCode
  | ToCharCode
  | StringLen
  | StringConcat
  (* Object operators *)
  | ObjectToList
  | ObjectFields
  (* List operators *)
  | ListHead
  | ListTail
  | ListLen
  | ListReverse
  (* Math operators *)
  | Abs
  | Sqrt
  | Ceil
  | Floor
  | Trunc

type binopt =
  (* Arithmetic operators *)
  | Plus
  | Minus
  | Times
  | Div
  | Modulo
  | Pow
  (* Bitwise operators *)
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | ShiftLeft
  | ShiftRight
  | ShiftRightLogical
  (* Logical operators *)
  | LogicalAnd
  | LogicalOr
  | SCLogicalAnd
  | SCLogicalOr
  (* Comparison operators *)
  | Eq
  | NE
  | Lt
  | Gt
  | Le
  | Ge
  (* Object operators *)
  | ObjectMem
  (* String operators *)
  | StringNth
  (* List operators *)
  | ListNth
  | ListAdd
  | ListPrepend
  | ListConcat

type triopt =
  (* General operators *)
  | ITE
  (* String operators *)
  | StringSubstr
  (* List operators *)
  | ListSet

type nopt =
  (* Logical operators *)
  | NAryLogicalAnd
  | NAryLogicalOr
  (* List operators *)
  | ListExpr

let is_infix_unopt (op : unopt) : bool =
  match op with BitwiseNot | LogicalNot -> true | _ -> false

let is_infix_binopt (op : binopt) : bool =
  match op with
  | Plus | Minus | Times | Div | Modulo | Pow | BitwiseAnd | BitwiseOr
  | BitwiseXor | ShiftLeft | ShiftRight | ShiftRightLogical | LogicalAnd
  | LogicalOr | SCLogicalAnd | SCLogicalOr | Eq | NE | Lt | Gt | Le | Ge
  | ObjectMem ->
    true
  | _ -> false

let label_of_const (c : const) : string =
  match c with
  | MAX_VALUE -> "Const.MAX_VALUE"
  | MIN_VALUE -> "Const.MIN_VALUE"
  | PI -> "Const.PI"

let label_of_unopt (op : unopt) : string =
  match op with
  | Neg -> "Arith.neg (-)"
  | BitwiseNot -> "Bitwise.not (~)"
  | LogicalNot -> "Logical.not (!)"
  | IntToFloat -> "Integer.int_to_float"
  | IntToString -> "Integer.int_to_string"
  | FloatToInt -> "Float.float_to_int"
  | FloatToString -> "Float.float_to_string"
  | StringToInt -> "String.string_to_int"
  | StringToFloat -> "String.string_to_float"
  | FromCharCode -> "String.from_char_code"
  | ToCharCode -> "String.to_char_code_u"
  | StringLen -> "String.s_len"
  | StringConcat -> "String.s_concat"
  | ObjectToList -> "Object.obj_to_list"
  | ObjectFields -> "Object.obj_fields"
  | ListHead -> "List.hd"
  | ListTail -> "List.tl"
  | ListLen -> "List.l_len"
  | ListReverse -> "List.l_reverse"
  | Abs -> "Math.abs"
  | Sqrt -> "Math.sqrt"
  | Ceil -> "Math.ceil"
  | Floor -> "Math.floor"
  | Trunc -> "Math.trunc"

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
  | SCLogicalAnd -> "Logical.sc_and (&&&)"
  | SCLogicalOr -> "Logical.sc_or (|||)"
  | Eq -> "Comp.eq (=)"
  | NE -> "Comp.ne (!=)"
  | Lt -> "Comp.lt (<)"
  | Gt -> "Comp.gt (>)"
  | Le -> "Comp.le (<=)"
  | Ge -> "Comp.ge (>=)"
  | ObjectMem -> "Object.in_obj"
  | StringNth -> "String.s_nth"
  | ListNth -> "List.l_nth"
  | ListAdd -> "List.l_add"
  | ListPrepend -> "List.l_prepend"
  | ListConcat -> "List.l_concat"

let label_of_triopt (op : triopt) : string =
  match op with
  | ITE -> "IfThenElse"
  | StringSubstr -> "String.s_substr"
  | ListSet -> "List.l_set"

let label_of_nopt (op : nopt) : string =
  match op with
  | NAryLogicalAnd -> "Logical.nary_and"
  | NAryLogicalOr -> "Logical.nary_or"
  | ListExpr -> "List.l_expr"

let pp_of_unopt_single (ppf : Fmt.t) (op : unopt) : unit =
  let open Fmt in
  match op with
  | Neg -> pp_str ppf "-"
  | BitwiseNot -> pp_str ppf "~"
  | LogicalNot -> pp_str ppf "!"
  | IntToFloat -> pp_str ppf "int_to_float"
  | IntToString -> pp_str ppf "int_to_string"
  | FloatToInt -> pp_str ppf "int_of_float"
  | FloatToString -> pp_str ppf "float_to_string"
  | StringToInt -> pp_str ppf "int_of_string"
  | StringToFloat -> pp_str ppf "float_of_string"
  | FromCharCode -> pp_str ppf "from_char_code"
  | ToCharCode -> pp_str ppf "to_char_code"
  | StringLen -> pp_str ppf "s_len"
  | StringConcat -> pp_str ppf "s_concat"
  | ObjectToList -> pp_str ppf "obj_to_list"
  | ObjectFields -> pp_str ppf "obj_fields"
  | ListHead -> pp_str ppf "hd"
  | ListTail -> pp_str ppf "tl"
  | ListLen -> pp_str ppf "l_len"
  | ListReverse -> pp_str ppf "l_reverse"
  | Abs -> pp_str ppf "abs"
  | Sqrt -> pp_str ppf "sqrt"
  | Ceil -> pp_str ppf "ceil"
  | Floor -> pp_str ppf "floor"
  | Trunc -> pp_str ppf "trunc"

let pp_of_binopt_single (ppf : Fmt.t) (op : binopt) : unit =
  let open Fmt in
  match op with
  | Plus -> pp_str ppf "+"
  | Minus -> pp_str ppf "-"
  | Times -> pp_str ppf "*"
  | Div -> pp_str ppf "/"
  | Modulo -> fmt ppf "%%"
  | Pow -> pp_str ppf "**"
  | BitwiseAnd -> pp_str ppf "&"
  | BitwiseOr -> pp_str ppf "|"
  | BitwiseXor -> pp_str ppf "^"
  | ShiftLeft -> pp_str ppf "<<"
  | ShiftRight -> pp_str ppf ">>"
  | ShiftRightLogical -> pp_str ppf ">>>"
  | LogicalAnd -> pp_str ppf "&&"
  | LogicalOr -> pp_str ppf "||"
  | SCLogicalAnd -> pp_str ppf "&&&"
  | SCLogicalOr -> pp_str ppf "|||"
  | Eq -> pp_str ppf "="
  | NE -> pp_str ppf "!="
  | Lt -> pp_str ppf "<"
  | Gt -> pp_str ppf ">"
  | Le -> pp_str ppf "<="
  | Ge -> pp_str ppf ">="
  | ObjectMem -> pp_str ppf "in_obj"
  | StringNth -> pp_str ppf "s_nth"
  | ListNth -> pp_str ppf "l_nth"
  | ListAdd -> pp_str ppf "l_add"
  | ListPrepend -> pp_str ppf "l_prepend"
  | ListConcat -> pp_str ppf "l_concat"

let pp_of_triopt_single (ppf : Fmt.t) (op : triopt) : unit =
  let open Fmt in
  match op with
  | ITE -> pp_str ppf "ite"
  | StringSubstr -> pp_str ppf "s_substr"
  | ListSet -> pp_str ppf "l_set"

let pp_of_const (ppf : Fmt.t) (c : const) : unit =
  let open Fmt in
  match c with
  | MAX_VALUE -> pp_str ppf "MAX_VALUE"
  | MIN_VALUE -> pp_str ppf "MIN_VALUE"
  | PI -> pp_str ppf "PI"

let pp_of_unopt (pp_val : Fmt.t -> 'a -> unit) (ppf : Fmt.t)
  ((op, v) : unopt * 'a) : unit =
  if is_infix_unopt op then Fmt.fmt ppf "%a%a" pp_of_unopt_single op pp_val v
  else Fmt.fmt ppf "%a(%a)" pp_of_unopt_single op pp_val v

let pp_of_binopt (pp_val : Fmt.t -> 'a -> unit) (ppf : Fmt.t)
  ((op, v1, v2) : binopt * 'a * 'a) : unit =
  if is_infix_binopt op then
    Fmt.fmt ppf "%a %a %a" pp_val v1 pp_of_binopt_single op pp_val v2
  else Fmt.fmt ppf "%a(%a, %a)" pp_of_binopt_single op pp_val v1 pp_val v2

let pp_of_triopt (pp_val : Fmt.t -> 'a -> unit) (ppf : Fmt.t)
  ((op, v1, v2, v3) : triopt * 'a * 'a * 'a) : unit =
  Fmt.fmt ppf "%a(%a, %a, %a)" pp_of_triopt_single op pp_val v1 pp_val v2 pp_val
    v3

let pp_of_nopt (pp_val : Fmt.t -> 'a -> unit) (ppf : Fmt.t)
  ((op, vs) : nopt * 'a list) : unit =
  let open Fmt in
  match op with
  | NAryLogicalAnd -> fmt ppf "%a" (pp_lst !>" && " pp_val) vs
  | NAryLogicalOr -> fmt ppf "%a" (pp_lst !>" || " pp_val) vs
  | ListExpr -> fmt ppf "[%a]" (pp_lst !>", " pp_val) vs

let str_of_unopt_single (op : unopt) : string =
  Fmt.str "%a" pp_of_unopt_single op

let str_of_binopt_single (op : binopt) : string =
  Fmt.str "%a" pp_of_binopt_single op

let str_of_triopt_single (op : triopt) : string =
  Fmt.str "%a" pp_of_triopt_single op

let str_of_const (c : const) : string = Fmt.str "%a" pp_of_const c

let str_of_unopt (pp_val : Fmt.t -> 'a -> unit) (op : unopt) (v : 'a) : string =
  Fmt.str "%a" (pp_of_unopt pp_val) (op, v)

let str_of_binopt (pp_val : Fmt.t -> 'a -> unit) (op : binopt) (v1 : 'a)
  (v2 : 'a) : string =
  Fmt.str "%a" (pp_of_binopt pp_val) (op, v1, v2)

let str_of_triopt (pp_val : Fmt.t -> 'a -> unit) (op : triopt) (v1 : 'a)
  (v2 : 'a) (v3 : 'a) : string =
  Fmt.str "%a" (pp_of_triopt pp_val) (op, v1, v2, v3)

let str_of_nopt (pp_val : Fmt.t -> 'a -> unit) (op : nopt) (vs : 'a list) :
  string =
  Fmt.str "%a" (pp_of_nopt pp_val) (op, vs)
