type funcPrototype_t = E_Type.t list * E_Type.t

let type_unop (op : Operator.unopt) : funcPrototype_t list =
  let notImplemented = [ ([ E_Type.AnyType ], E_Type.AnyType) ] in
  match op with
  | Operator.Neg ->
    [ ([ E_Type.IntType ], E_Type.IntType)
    ; ([ E_Type.FloatType ], E_Type.FloatType)
    ]
  | Operator.LogicalNot -> [ ([ E_Type.BooleanType ], E_Type.BooleanType) ]
  | Operator.BitwiseNot -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.IsNaN -> [ ([ E_Type.AnyType ], E_Type.BooleanType) ]
  | Operator.Typeof ->
    [ ([ E_Type.AnyType ], E_Type.RuntimeType Type.TypeType) ]
  | Operator.ToInt -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.ToInt32 -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.ToUint16 -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.ToUint32 -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.IntToFloat -> [ ([ E_Type.IntType ], E_Type.FloatType) ]
  | Operator.FloatToInt -> [ ([ E_Type.FloatType ], E_Type.IntType) ]
  | Operator.IntToString -> [ ([ E_Type.IntType ], E_Type.StringType) ]
  | Operator.StringToInt -> [ ([ E_Type.StringType ], E_Type.IntType) ]
  | Operator.IntToFourHex -> [ ([ E_Type.IntType ], E_Type.StringType) ]
  | Operator.OctalToDecimal -> [ ([ E_Type.IntType ], E_Type.IntType) ]
  | Operator.FloatToString -> [ ([ E_Type.FloatType ], E_Type.StringType) ]
  | Operator.StringToFloat -> [ ([ E_Type.StringType ], E_Type.FloatType) ]
  | Operator.FloatToByte -> notImplemented
  | Operator.Float32ToLEBytes -> notImplemented
  | Operator.Float32ToBEBytes -> notImplemented
  | Operator.Float64ToLEBytes -> notImplemented
  | Operator.Float64ToBEBytes -> notImplemented
  | Operator.Float32FromLEBytes -> notImplemented
  | Operator.Float32FromBEBytes -> notImplemented
  | Operator.Float64FromLEBytes -> notImplemented
  | Operator.Float64FromBEBytes -> notImplemented
  | Operator.BytesToString -> notImplemented
  | Operator.Utf8Decode -> [ ([ E_Type.StringType ], E_Type.StringType) ]
  | Operator.HexDecode -> [ ([ E_Type.StringType ], E_Type.StringType) ]
  | Operator.FromCharCode -> [ ([ E_Type.IntType ], E_Type.StringType) ]
  | Operator.FromCharCodeU -> [ ([ E_Type.IntType ], E_Type.StringType) ]
  | Operator.ToCharCode -> [ ([ E_Type.StringType ], E_Type.IntType) ]
  | Operator.ToCharCodeU -> [ ([ E_Type.StringType ], E_Type.IntType) ]
  | Operator.ToLowerCase -> [ ([ E_Type.StringType ], E_Type.StringType) ]
  | Operator.ToUpperCase -> [ ([ E_Type.StringType ], E_Type.StringType) ]
  | Operator.Trim -> [ ([ E_Type.StringType ], E_Type.FloatType) ]
  | Operator.Random -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Abs -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Sqrt -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Ceil -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Floor -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Exp -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Log2 -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.LogE -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Log10 -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Sin -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Cos -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Tan -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Sinh -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Cosh -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Tanh -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Acos -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Asin -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Atan -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.ParseNumber -> [ ([ E_Type.StringType ], E_Type.StringType) ]
  | Operator.ParseString -> [ ([ E_Type.StringType ], E_Type.StringType) ]
  | Operator.ParseDate -> notImplemented
  | Operator.StringLen -> [ ([ E_Type.StringType ], E_Type.IntType) ]
  | Operator.StringLenU -> [ ([ E_Type.StringType ], E_Type.IntType) ]
  | Operator.StringConcat -> notImplemented
  | Operator.ArrayLen -> notImplemented
  | Operator.ListToArray -> notImplemented
  | Operator.ListHead -> notImplemented
  | Operator.ListTail -> notImplemented
  | Operator.ListLen -> notImplemented
  | Operator.ListSort -> notImplemented
  | Operator.ListReverse -> notImplemented
  | Operator.ListRemoveLast -> notImplemented
  | Operator.TupleFirst -> notImplemented
  | Operator.TupleSecond -> notImplemented
  | Operator.TupleLen -> notImplemented
  | Operator.ObjectToList -> notImplemented
  | Operator.ObjectFields -> notImplemented

let type_binop (op : Operator.binopt) : funcPrototype_t list =
  let notImplemented =
    [ ([ E_Type.AnyType; E_Type.AnyType ], E_Type.AnyType) ]
  in
  match op with
  | Operator.Plus ->
    [ ([ E_Type.IntType; E_Type.IntType ], E_Type.IntType)
    ; ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType)
    ]
  | Operator.Minus ->
    [ ([ E_Type.IntType; E_Type.IntType ], E_Type.IntType)
    ; ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType)
    ]
  | Operator.Times ->
    [ ([ E_Type.IntType; E_Type.IntType ], E_Type.IntType)
    ; ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType)
    ]
  | Operator.Div ->
    [ ([ E_Type.IntType; E_Type.IntType ], E_Type.IntType)
    ; ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType)
    ]
  | Operator.Modulo ->
    [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.BitwiseAnd ->
    [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.BitwiseOr ->
    [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.BitwiseXor ->
    [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.ShiftLeft ->
    [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.ShiftRight ->
    [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.ShiftRightLogical ->
    [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.LogicalAnd ->
    [ ([ E_Type.BooleanType; E_Type.BooleanType ], E_Type.BooleanType) ]
  | Operator.LogicalOr ->
    [ ([ E_Type.BooleanType; E_Type.BooleanType ], E_Type.BooleanType) ]
  | Operator.Eq -> [ ([ E_Type.AnyType; E_Type.AnyType ], E_Type.BooleanType) ]
  | Operator.Lt -> [ ([ E_Type.AnyType; E_Type.AnyType ], E_Type.BooleanType) ]
  | Operator.Gt -> [ ([ E_Type.AnyType; E_Type.AnyType ], E_Type.BooleanType) ]
  | Operator.Le -> [ ([ E_Type.AnyType; E_Type.AnyType ], E_Type.BooleanType) ]
  | Operator.Ge -> [ ([ E_Type.AnyType; E_Type.AnyType ], E_Type.BooleanType) ]
  | Operator.Min ->
    [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Max ->
    [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Pow ->
    [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.Atan2 ->
    [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operator.IntToBEBytes -> notImplemented
  | Operator.IntFromLEBytes -> notImplemented
  | Operator.UintFromLEBytes -> notImplemented
  | Operator.ToPrecision ->
    [ ([ E_Type.FloatType; E_Type.IntType ], E_Type.StringType) ]
  | Operator.ToExponential ->
    [ ([ E_Type.FloatType; E_Type.IntType ], E_Type.StringType) ]
  | Operator.ToFixed ->
    [ ([ E_Type.FloatType; E_Type.IntType ], E_Type.StringType) ]
  | Operator.ObjectMem -> notImplemented
  | Operator.StringNth ->
    [ ([ E_Type.StringType; E_Type.IntType ], E_Type.StringType) ]
  | Operator.StringNthU ->
    [ ([ E_Type.StringType; E_Type.IntType ], E_Type.StringType) ]
  | Operator.StringSplit -> notImplemented
  | Operator.ArrayMake -> notImplemented
  | Operator.ArrayNth -> notImplemented
  | Operator.ListNth -> notImplemented
  | Operator.ListMem -> notImplemented
  | Operator.ListAdd -> notImplemented
  | Operator.ListPrepend -> notImplemented
  | Operator.ListConcat -> notImplemented
  | Operator.ListRemove -> notImplemented
  | Operator.ListRemoveNth -> notImplemented
  | Operator.TupleNth -> notImplemented

let type_ebinop (op : E_Operator.binopt) : funcPrototype_t list =
  match op with
  | E_Operator.SCLogicalAnd ->
    [ ([ E_Type.BooleanType; E_Type.BooleanType ], E_Type.BooleanType) ]
  | E_Operator.SCLogicalOr ->
    [ ([ E_Type.BooleanType; E_Type.BooleanType ], E_Type.BooleanType) ]

let type_triop (op : Operator.triopt) : funcPrototype_t list =
  let notImplemented =
    [ ([ E_Type.AnyType; E_Type.AnyType; E_Type.AnyType ], E_Type.AnyType) ]
  in
  match op with
  | Operator.StringSubstr ->
    [ ([ E_Type.StringType; E_Type.IntType; E_Type.IntType ], E_Type.StringType)
    ]
  | Operator.StringSubstrU ->
    [ ([ E_Type.StringType; E_Type.IntType; E_Type.IntType ], E_Type.StringType)
    ]
  | Operator.ArraySet | Operator.ListSet | Operator.ITE -> notImplemented
