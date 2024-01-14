type funcPrototype_t = EType.t list * EType.t

let type_unop (op : Operator.unopt) : funcPrototype_t list =
  let notImplemented = [ ([ EType.AnyType ], EType.AnyType) ] in
  match op with
  | Operator.Neg ->
    [ ([ EType.IntType ], EType.IntType)
    ; ([ EType.FloatType ], EType.FloatType)
    ]
  | Operator.LogicalNot -> [ ([ EType.BooleanType ], EType.BooleanType) ]
  | Operator.BitwiseNot -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.IsNaN -> [ ([ EType.AnyType ], EType.BooleanType) ]
  | Operator.Typeof -> [ ([ EType.AnyType ], EType.RuntimeType Type.TypeType) ]
  | Operator.ToInt -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.ToInt32 -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.ToUint16 -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.ToUint32 -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.IntToFloat -> [ ([ EType.IntType ], EType.FloatType) ]
  | Operator.FloatToInt -> [ ([ EType.FloatType ], EType.IntType) ]
  | Operator.IntToString -> [ ([ EType.IntType ], EType.StringType) ]
  | Operator.StringToInt -> [ ([ EType.StringType ], EType.IntType) ]
  | Operator.IntToFourHex -> [ ([ EType.IntType ], EType.StringType) ]
  | Operator.OctalToDecimal -> [ ([ EType.IntType ], EType.IntType) ]
  | Operator.FloatToString -> [ ([ EType.FloatType ], EType.StringType) ]
  | Operator.StringToFloat -> [ ([ EType.StringType ], EType.FloatType) ]
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
  | Operator.Utf8Decode -> [ ([ EType.StringType ], EType.StringType) ]
  | Operator.HexDecode -> [ ([ EType.StringType ], EType.StringType) ]
  | Operator.FromCharCode -> [ ([ EType.IntType ], EType.StringType) ]
  | Operator.FromCharCodeU -> [ ([ EType.IntType ], EType.StringType) ]
  | Operator.ToCharCode -> [ ([ EType.StringType ], EType.IntType) ]
  | Operator.ToCharCodeU -> [ ([ EType.StringType ], EType.IntType) ]
  | Operator.ToLowerCase -> [ ([ EType.StringType ], EType.StringType) ]
  | Operator.ToUpperCase -> [ ([ EType.StringType ], EType.StringType) ]
  | Operator.Trim -> [ ([ EType.StringType ], EType.FloatType) ]
  | Operator.Random -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Abs -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Sqrt -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Ceil -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Floor -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Exp -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Log2 -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.LogE -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Log10 -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Sin -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Cos -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Tan -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Sinh -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Cosh -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Tanh -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Acos -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Asin -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.Atan -> [ ([ EType.FloatType ], EType.FloatType) ]
  | Operator.ParseNumber -> [ ([ EType.StringType ], EType.StringType) ]
  | Operator.ParseString -> [ ([ EType.StringType ], EType.StringType) ]
  | Operator.ParseDate -> notImplemented
  | Operator.StringLen -> [ ([ EType.StringType ], EType.IntType) ]
  | Operator.StringLenU -> [ ([ EType.StringType ], EType.IntType) ]
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
  let notImplemented = [ ([ EType.AnyType; EType.AnyType ], EType.AnyType) ] in
  match op with
  | Operator.Plus ->
    [ ([ EType.IntType; EType.IntType ], EType.IntType)
    ; ([ EType.FloatType; EType.FloatType ], EType.FloatType)
    ]
  | Operator.Minus ->
    [ ([ EType.IntType; EType.IntType ], EType.IntType)
    ; ([ EType.FloatType; EType.FloatType ], EType.FloatType)
    ]
  | Operator.Times ->
    [ ([ EType.IntType; EType.IntType ], EType.IntType)
    ; ([ EType.FloatType; EType.FloatType ], EType.FloatType)
    ]
  | Operator.Div ->
    [ ([ EType.IntType; EType.IntType ], EType.IntType)
    ; ([ EType.FloatType; EType.FloatType ], EType.FloatType)
    ]
  | Operator.Modulo ->
    [ ([ EType.FloatType; EType.FloatType ], EType.FloatType) ]
  | Operator.BitwiseAnd ->
    [ ([ EType.FloatType; EType.FloatType ], EType.FloatType) ]
  | Operator.BitwiseOr ->
    [ ([ EType.FloatType; EType.FloatType ], EType.FloatType) ]
  | Operator.BitwiseXor ->
    [ ([ EType.FloatType; EType.FloatType ], EType.FloatType) ]
  | Operator.ShiftLeft ->
    [ ([ EType.FloatType; EType.FloatType ], EType.FloatType) ]
  | Operator.ShiftRight ->
    [ ([ EType.FloatType; EType.FloatType ], EType.FloatType) ]
  | Operator.ShiftRightLogical ->
    [ ([ EType.FloatType; EType.FloatType ], EType.FloatType) ]
  | Operator.LogicalAnd ->
    [ ([ EType.BooleanType; EType.BooleanType ], EType.BooleanType) ]
  | Operator.LogicalOr ->
    [ ([ EType.BooleanType; EType.BooleanType ], EType.BooleanType) ]
  | Operator.SCLogicalAnd ->
    [ ([ EType.BooleanType; EType.BooleanType ], EType.BooleanType) ]
  | Operator.SCLogicalOr ->
    [ ([ EType.BooleanType; EType.BooleanType ], EType.BooleanType) ]
  | Operator.Eq -> [ ([ EType.AnyType; EType.AnyType ], EType.BooleanType) ]
  | Operator.Lt -> [ ([ EType.AnyType; EType.AnyType ], EType.BooleanType) ]
  | Operator.Gt -> [ ([ EType.AnyType; EType.AnyType ], EType.BooleanType) ]
  | Operator.Le -> [ ([ EType.AnyType; EType.AnyType ], EType.BooleanType) ]
  | Operator.Ge -> [ ([ EType.AnyType; EType.AnyType ], EType.BooleanType) ]
  | Operator.Min -> [ ([ EType.FloatType; EType.FloatType ], EType.FloatType) ]
  | Operator.Max -> [ ([ EType.FloatType; EType.FloatType ], EType.FloatType) ]
  | Operator.Pow -> [ ([ EType.FloatType; EType.FloatType ], EType.FloatType) ]
  | Operator.Atan2 ->
    [ ([ EType.FloatType; EType.FloatType ], EType.FloatType) ]
  | Operator.IntToBEBytes -> notImplemented
  | Operator.IntFromLEBytes -> notImplemented
  | Operator.UintFromLEBytes -> notImplemented
  | Operator.ToPrecision ->
    [ ([ EType.FloatType; EType.IntType ], EType.StringType) ]
  | Operator.ToExponential ->
    [ ([ EType.FloatType; EType.IntType ], EType.StringType) ]
  | Operator.ToFixed ->
    [ ([ EType.FloatType; EType.IntType ], EType.StringType) ]
  | Operator.ObjectMem -> notImplemented
  | Operator.StringNth ->
    [ ([ EType.StringType; EType.IntType ], EType.StringType) ]
  | Operator.StringNthU ->
    [ ([ EType.StringType; EType.IntType ], EType.StringType) ]
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

let type_triop (op : Operator.triopt) : funcPrototype_t list =
  let notImplemented =
    [ ([ EType.AnyType; EType.AnyType; EType.AnyType ], EType.AnyType) ]
  in
  match op with
  | Operator.StringSubstr ->
    [ ([ EType.StringType; EType.IntType; EType.IntType ], EType.StringType) ]
  | Operator.StringSubstrU ->
    [ ([ EType.StringType; EType.IntType; EType.IntType ], EType.StringType) ]
  | Operator.ArraySet | Operator.ListSet | Operator.ITE -> notImplemented
