open EslBase

type unopt =
  | Neg
  | BitwiseNot
  | LogicalNot
  | IntToFloat
  | IntToString
  | FloatToInt
  | FloatToString
  | StringToInt
  | StringToFloat
  | ListHead
  | ListTail
  (* Temp operators *)
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
  (* Temp operators *)
  | ObjectMem

type triopt = Conditional
(* Temp operators *)

type nopt =
  | ListExpr
  (* Temp operators *)
  | NAryLogicalAnd
  | NAryLogicalOr

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
  | ListHead -> "List.hd"
  | ListTail -> "List.tl"
  | ObjectToList -> "Object.obj_to_list"
  | ObjectFields -> "Object.obj_fields"

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
  | Eq -> "Comp.eq (==)"
  | Ne -> "Comp.ne (!=)"
  | Lt -> "Comp.lt (<)"
  | Gt -> "Comp.gt (>)"
  | Le -> "Comp.le (<=)"
  | Ge -> "Comp.ge (>=)"
  | ObjectMem -> "Object.in_obj"

let label_of_triopt (op : triopt) : string =
  match op with Conditional -> "Conditional"

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
  | FloatToInt -> pp_str ppf "float_to_int"
  | FloatToString -> pp_str ppf "float_to_string"
  | StringToInt -> pp_str ppf "string_to_int"
  | StringToFloat -> pp_str ppf "string_to_float"
  | ObjectToList -> pp_str ppf "obj_to_list"
  | ObjectFields -> pp_str ppf "obj_fields"
  | ListHead -> pp_str ppf "hd"
  | ListTail -> pp_str ppf "tl"

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
  | Eq -> pp_str ppf "=="
  | Ne -> pp_str ppf "!="
  | Lt -> pp_str ppf "<"
  | Gt -> pp_str ppf ">"
  | Le -> pp_str ppf "<="
  | Ge -> pp_str ppf ">="
  | ObjectMem -> pp_str ppf "in_obj"

let pp_of_triopt_single (ppf : Fmt.t) (op : triopt) : unit =
  let open Fmt in
  match op with Conditional -> pp_str ppf "?:"

let pp_of_unopt (pp_val : Fmt.t -> 'a -> unit) (ppf : Fmt.t)
  ((op, v) : unopt * 'a) : unit =
  Fmt.fmt ppf "%a%a" pp_of_unopt_single op pp_val v

let pp_of_binopt (pp_val : Fmt.t -> 'a -> unit) (ppf : Fmt.t)
  ((op, v1, v2) : binopt * 'a * 'a) : unit =
  Fmt.fmt ppf "%a %a %a" pp_val v1 pp_of_binopt_single op pp_val v2

let pp_of_triopt (pp_val : Fmt.t -> 'a -> unit) (ppf : Fmt.t)
  ((op, v1, v2, v3) : triopt * 'a * 'a * 'a) : unit =
  match op with
  | Conditional -> Fmt.fmt ppf "%a ? %a : %a" pp_val v1 pp_val v2 pp_val v3

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
