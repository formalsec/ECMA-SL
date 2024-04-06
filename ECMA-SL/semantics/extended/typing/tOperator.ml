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
  | FloatToInt -> type_op [ ([ FloatType ], IntType) ]
  | FloatToString -> type_op [ ([ FloatType ], StringType) ]
  | StringToInt -> type_op [ ([ StringType ], IntType) ]
  | StringToFloat -> type_op [ ([ StringType ], FloatType) ]
  | FromCharCode -> type_op [ ([ IntType ], StringType) ]
  | ToCharCode -> type_op [ ([ StringType ], IntType) ]
  | StringLen -> type_op [ ([ StringType ], IntType) ]
  | StringConcat -> not_implemented (* TODO: list typing *)
  | ObjectToList -> not_implemented (* TODO: custom object typing function *)
  | ObjectFields -> not_implemented (* TODO: custom object typing function *)
  | ListHead -> not_implemented (* TODO: list typing *)
  | ListTail -> not_implemented (* TODO: list typing *)
  | ListLen -> not_implemented (* TODO: list typing *)
  | ListReverse -> not_implemented (* TODO: list typing *)
  | TupleFirst -> not_implemented (* TODO: tuple typing *)
  | TupleSecond -> not_implemented (* TODO: tuple typing *)
  | TupleLen -> not_implemented (* TODO: tuple typing *)
  | Random -> type_op [ ([ FloatType ], FloatType) ]
  | Abs -> type_op [ ([ FloatType ], FloatType) ]
  | Sqrt -> type_op [ ([ FloatType ], FloatType) ]
  | Ceil -> type_op [ ([ FloatType ], FloatType) ]
  | Floor -> type_op [ ([ FloatType ], FloatType) ]
  | Trunc -> type_op [ ([ FloatType ], FloatType) ]
  | Exp -> type_op [ ([ FloatType ], FloatType) ]

let type_binopt (op : Operator.binopt) (targs : EType.t list) : EType.t' =
  let not_implemented = EType.AnyType in
  let type_op = type_operator targs in
  let int_arith = EType.([ IntType; IntType ], IntType) in
  let float_arith = EType.([ FloatType; FloatType ], FloatType) in
  match op with
  | Plus ->
    type_op [ int_arith; float_arith; ([ StringType; StringType ], StringType) ]
  | Minus -> type_op [ int_arith; float_arith ]
  | Times -> type_op [ int_arith; float_arith ]
  | Div -> type_op [ int_arith; float_arith ]
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
  | ObjectMem -> not_implemented (* TODO: custom object typing function *)
  | StringNth -> type_op [ ([ StringType; IntType ], StringType) ]
  | ListNth -> not_implemented (* TODO: list typing *)
  | ListAdd -> not_implemented (* TODO: list typing *)
  | ListPrepend -> not_implemented (* TODO: list typing *)
  | ListConcat -> not_implemented (* TODO: list typing *)
  | TupleNth -> not_implemented (* TODO: tuple typing *)

let type_triopt (op : Operator.triopt) (targs : EType.t list) : EType.t' =
  let not_implemented = EType.AnyType in
  let type_op = type_operator targs in
  match op with
  | ITE -> not_implemented (* TODO: custom if-then-else typing function *)
  | StringSubstr -> type_op [ ([ StringType; IntType; IntType ], StringType) ]
  | ListSet -> not_implemented (* TODO: list typing *)

let type_nopt (op : Operator.nopt) (_targs : EType.t list) : EType.t' =
  let not_implemented = EType.AnyType in
  match op with
  | NAryLogicalAnd -> not_implemented (* TODO: nopt typing *)
  | NAryLogicalOr -> not_implemented (* TODO: nopt typing *)
  | ArrayExpr -> not_implemented (* TODO: nopt typing *)
  | ListExpr -> not_implemented (* TODO: nopt typing *)
  | TupleExpr -> not_implemented (* TODO: nopt typing *)
