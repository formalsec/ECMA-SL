open EslBase
open EslSyntax

let ( ~@ ) (t : EType.t') : EType.t = Source.(t @?> no_region)

type op_signature = (EType.t' list * EType.t') list

let type_check_signature (targs : EType.t list) (tpxs : EType.t' list) : unit =
  let type_check_operand (tpx, targ) =
    try TSubtyping.type_check ~@tpx targ
    with Typing_error.Error err ->
      Typing_error.(update (BadOperand (~@tpx, targ)) err |> raise)
  in
  List.combine tpxs targs |> List.iter type_check_operand

let rec type_operator_strict ?(err : Typing_error.t option = None)
  (targs : EType.t list) (op_sig : op_signature) : EType.t' =
  match (op_sig, err) with
  | ([], None) -> Internal_error.(throw __FUNCTION__ (Expecting "type error"))
  | ([], Some err') -> Typing_error.raise err'
  | ((tpxs, tret) :: op_sig', _) -> (
    try type_check_signature targs tpxs |> fun () -> tret
    with Typing_error.Error err' ->
      let err = Some (Option.value ~default:err' err) in
      type_operator_strict ~err targs op_sig' )

let type_operator (targs : EType.t list) (op_sig : op_signature) : EType.t' =
  let has_tany_f = EType.equal ~@AnyType in
  if List.length op_sig > 1 && List.exists has_tany_f targs then AnyType
  else type_operator_strict targs op_sig

let type_const (c : Operator.const) : EType.t' =
  match c with
  | MAX_VALUE -> FloatType
  | MIN_VALUE -> FloatType
  | PI -> FloatType

let type_unopt (op : Operator.unopt) (targs : EType.t list) : EType.t' =
  let not_implemented = EType.AnyType in
  let type_op = type_operator targs in
  match op with
  | Typeof -> not_implemented
  | Neg -> type_op [ ([ IntType ], IntType); ([ FloatType ], FloatType) ]
  | BitwiseNot -> type_op [ ([ FloatType ], FloatType) ]
  | LogicalNot -> type_op [ ([ BooleanType ], BooleanType) ]
  | IntToFloat -> type_op [ ([ IntType ], FloatType) ]
  | IntToString -> type_op [ ([ IntType ], StringType) ]
  | IntToFourHex -> type_op [ ([ IntType ], StringType) ]
  | OctalToDecimal -> type_op [ ([ IntType ], IntType) ]
  | FloatToInt -> type_op [ ([ FloatType ], IntType) ]
  | FloatToString -> type_op [ ([ FloatType ], StringType) ]
  | ToInt -> type_op [ ([ FloatType ], FloatType) ]
  | ToInt32 -> type_op [ ([ FloatType ], FloatType) ]
  | ToUint16 -> type_op [ ([ FloatType ], FloatType) ]
  | ToUint32 -> type_op [ ([ FloatType ], FloatType) ]
  | IsNaN -> type_op [ ([ UnknownType ], BooleanType) ]
  | StringToInt -> type_op [ ([ StringType ], IntType) ]
  | StringToFloat -> type_op [ ([ StringType ], FloatType) ]
  | FromCharCode -> type_op [ ([ IntType ], StringType) ]
  | FromCharCodeU -> type_op [ ([ IntType ], StringType) ]
  | ToCharCode -> type_op [ ([ StringType ], IntType) ]
  | ToCharCodeU -> type_op [ ([ StringType ], IntType) ]
  | ToLowerCase -> type_op [ ([ StringType ], StringType) ]
  | ToUpperCase -> type_op [ ([ StringType ], StringType) ]
  | Trim -> type_op [ ([ StringType ], StringType) ]
  | StringLen -> type_op [ ([ StringType ], IntType) ]
  | StringLenU -> type_op [ ([ StringType ], IntType) ]
  | StringConcat -> not_implemented (* TODO: list typing *)
  | ObjectToList -> not_implemented (* TODO: custom object typing function *)
  | ObjectFields -> not_implemented (* TODO: custom object typing function *)
  | ArrayLen -> not_implemented (* TODO: array typing *)
  | ListToArray -> not_implemented (* TODO: array typing *)
  | ListHead -> not_implemented (* TODO: list typing *)
  | ListTail -> not_implemented (* TODO: list typing *)
  | ListLen -> not_implemented (* TODO: list typing *)
  | ListSort -> not_implemented (* TODO: list typing *)
  | ListReverse -> not_implemented (* TODO: list typing *)
  | ListRemoveLast -> not_implemented (* TODO: list typing *)
  | TupleFirst -> not_implemented (* TODO: tuple typing *)
  | TupleSecond -> not_implemented (* TODO: tuple typing *)
  | TupleLen -> not_implemented (* TODO: tuple typing *)
  | FloatToByte -> not_implemented (* TODO: byte typing *)
  | Float32ToLEBytes -> not_implemented (* TODO: byte typing *)
  | Float32ToBEBytes -> not_implemented (* TODO: byte typing *)
  | Float64ToLEBytes -> not_implemented (* TODO: byte typing *)
  | Float64ToBEBytes -> not_implemented (* TODO: byte typing *)
  | Float32FromLEBytes -> not_implemented (* TODO: byte typing *)
  | Float32FromBEBytes -> not_implemented (* TODO: byte typing *)
  | Float64FromLEBytes -> not_implemented (* TODO: byte typing *)
  | Float64FromBEBytes -> not_implemented (* TODO: byte typing *)
  | BytesToString -> not_implemented (* TODO: byte typing *)
  | Random -> type_op [ ([ FloatType ], FloatType) ]
  | Abs -> type_op [ ([ FloatType ], FloatType) ]
  | Sqrt -> type_op [ ([ FloatType ], FloatType) ]
  | Ceil -> type_op [ ([ FloatType ], FloatType) ]
  | Floor -> type_op [ ([ FloatType ], FloatType) ]
  | Trunc -> type_op [ ([ FloatType ], FloatType) ]
  | Exp -> type_op [ ([ FloatType ], FloatType) ]
  | Log2 -> type_op [ ([ FloatType ], FloatType) ]
  | LogE -> type_op [ ([ FloatType ], FloatType) ]
  | Log10 -> type_op [ ([ FloatType ], FloatType) ]
  | Sin -> type_op [ ([ FloatType ], FloatType) ]
  | Cos -> type_op [ ([ FloatType ], FloatType) ]
  | Tan -> type_op [ ([ FloatType ], FloatType) ]
  | Sinh -> type_op [ ([ FloatType ], FloatType) ]
  | Cosh -> type_op [ ([ FloatType ], FloatType) ]
  | Tanh -> type_op [ ([ FloatType ], FloatType) ]
  | Asin -> type_op [ ([ FloatType ], FloatType) ]
  | Acos -> type_op [ ([ FloatType ], FloatType) ]
  | Atan -> type_op [ ([ FloatType ], FloatType) ]
  | Utf8Decode -> type_op [ ([ StringType ], StringType) ]
  | HexDecode -> type_op [ ([ StringType ], StringType) ]
  | ParseNumber -> type_op [ ([ StringType ], StringType) ]
  | ParseString -> type_op [ ([ StringType ], StringType) ]
  | ParseDate -> type_op [ ([ StringType ], StringType) ]

let type_binopt (op : Operator.binopt) (targs : EType.t list) : EType.t' =
  let not_implemented = EType.AnyType in
  let type_op = type_operator targs in
  let float_arith = EType.([ FloatType; FloatType ], FloatType) in
  match op with
  | Plus -> type_op [ ([ IntType; IntType ], IntType); float_arith ]
  | Minus -> type_op [ ([ IntType; IntType ], IntType); float_arith ]
  | Times -> type_op [ ([ IntType; IntType ], IntType); float_arith ]
  | Div -> type_op [ ([ IntType; IntType ], IntType); float_arith ]
  | Modulo -> type_op [ float_arith ]
  | Pow -> type_op [ float_arith ]
  | BitwiseAnd -> type_op [ float_arith ]
  | BitwiseOr -> type_op [ float_arith ]
  | BitwiseXor -> type_op [ float_arith ]
  | ShiftLeft -> type_op [ float_arith ]
  | ShiftRight -> type_op [ float_arith ]
  | ShiftRightLogical -> type_op [ float_arith ]
  | LogicalAnd -> type_op [ ([ BooleanType; BooleanType ], BooleanType) ]
  | LogicalOr -> type_op [ ([ BooleanType; BooleanType ], BooleanType) ]
  | SCLogicalAnd -> type_op [ ([ BooleanType; BooleanType ], BooleanType) ]
  | SCLogicalOr -> type_op [ ([ BooleanType; BooleanType ], BooleanType) ]
  | Eq | NE -> type_op [ ([ UnknownType; UnknownType ], BooleanType) ]
  | Lt -> type_op [ ([ UnknownType; UnknownType ], BooleanType) ]
  | Gt -> type_op [ ([ UnknownType; UnknownType ], BooleanType) ]
  | Le -> type_op [ ([ UnknownType; UnknownType ], BooleanType) ]
  | Ge -> type_op [ ([ UnknownType; UnknownType ], BooleanType) ]
  | ToPrecision -> type_op [ ([ FloatType; IntType ], StringType) ]
  | ToExponential -> type_op [ ([ FloatType; IntType ], StringType) ]
  | ToFixed -> type_op [ ([ FloatType; IntType ], StringType) ]
  | ObjectMem -> not_implemented (* TODO: custom object typing function *)
  | StringNth -> type_op [ ([ StringType; IntType ], StringType) ]
  | StringNthU -> type_op [ ([ StringType; IntType ], StringType) ]
  | StringSplit -> type_op [ ([ StringType; StringType ], StringType) ]
  | ArrayMake -> not_implemented (* TODO: array typing *)
  | ArrayNth -> not_implemented (* TODO: array typing *)
  | ListMem -> not_implemented (* TODO: list typing *)
  | ListNth -> not_implemented (* TODO: list typing *)
  | ListAdd -> not_implemented (* TODO: list typing *)
  | ListPrepend -> not_implemented (* TODO: list typing *)
  | ListConcat -> not_implemented (* TODO: list typing *)
  | ListRemove -> not_implemented (* TODO: list typing *)
  | ListRemoveNth -> not_implemented (* TODO: list typing *)
  | TupleNth -> not_implemented (* TODO: tuple typing *)
  | IntToBEBytes -> not_implemented (* TODO: byste typing *)
  | IntFromLEBytes -> not_implemented (* TODO: byte typing *)
  | UintFromLEBytes -> not_implemented (* TODO: byte typing *)
  | Min -> type_op [ float_arith ]
  | Max -> type_op [ float_arith ]
  | Atan2 -> type_op [ float_arith ]

let type_triopt (op : Operator.triopt) (targs : EType.t list) : EType.t' =
  let not_implemented = EType.AnyType in
  let type_op = type_operator targs in
  match op with
  | ITE -> not_implemented (* TODO: custom if-then-else typing function *)
  | StringSubstr -> type_op [ ([ StringType; IntType; IntType ], StringType) ]
  | StringSubstrU -> type_op [ ([ StringType; IntType; IntType ], StringType) ]
  | ArraySet -> not_implemented (* TODO: array typing *)
  | ListSet -> not_implemented (* TODO: list typing *)

let type_nopt (op : Operator.nopt) (_targs : EType.t list) : EType.t' =
  let not_implemented = EType.AnyType in
  match op with
  | NAryLogicalAnd -> not_implemented (* TODO: nopt typing *)
  | NAryLogicalOr -> not_implemented (* TODO: nopt typing *)
  | ArrayExpr -> not_implemented (* TODO: nopt typing *)
  | ListExpr -> not_implemented (* TODO: nopt typing *)
  | TupleExpr -> not_implemented (* TODO: nopt typing *)
