let type_unop (op : Operators.uopt) : E_Type.t list * E_Type.t =
  let notImplemented = ([ E_Type.AnyType ], E_Type.AnyType) in
  match op with
  | Operators.Neg -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Not -> ([ E_Type.BooleanType ], E_Type.BooleanType)
  | Operators.BitwiseNot -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.IsNaN -> ([ E_Type.NumberType ], E_Type.BooleanType)
  | Operators.Typeof -> ([ E_Type.AnyType ], E_Type.RuntimeType Type.TypeType)
  | Operators.ToInt -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.ToInt32 -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.ToUint16 -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.ToUint32 -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.IntToFloat -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.IntOfFloat -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.IntToString -> ([ E_Type.NumberType ], E_Type.StringType)
  | Operators.IntOfString -> ([ E_Type.StringType ], E_Type.NumberType)
  | Operators.IntToFourHex -> ([ E_Type.NumberType ], E_Type.StringType)
  | Operators.OctalToDecimal -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.FloatToString -> ([ E_Type.NumberType ], E_Type.StringType)
  | Operators.FloatOfString -> ([ E_Type.StringType ], E_Type.NumberType)
  | Operators.FloatToByte -> notImplemented
  | Operators.Float64ToLEBytes -> notImplemented
  | Operators.Float64ToBEBytes -> notImplemented
  | Operators.Float32ToLEBytes -> notImplemented
  | Operators.Float32ToBEBytes -> notImplemented
  | Operators.Float64FromLEBytes -> notImplemented
  | Operators.Float64FromBEBytes -> notImplemented
  | Operators.Float32FromLEBytes -> notImplemented
  | Operators.Float32FromBEBytes -> notImplemented
  | Operators.BytesToString -> notImplemented
  | Operators.Utf8Decode -> ([ E_Type.StringType ], E_Type.StringType)
  | Operators.HexDecode -> ([ E_Type.StringType ], E_Type.StringType)
  | Operators.FromCharCode -> ([ E_Type.NumberType ], E_Type.StringType)
  | Operators.FromCharCodeU -> ([ E_Type.NumberType ], E_Type.StringType)
  | Operators.ToCharCode -> ([ E_Type.StringType ], E_Type.NumberType)
  | Operators.ToCharCodeU -> ([ E_Type.StringType ], E_Type.NumberType)
  | Operators.ToLowerCase -> ([ E_Type.StringType ], E_Type.StringType)
  | Operators.ToUpperCase -> ([ E_Type.StringType ], E_Type.StringType)
  | Operators.Random -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Trim -> ([ E_Type.StringType ], E_Type.StringType)
  | Operators.Abs -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Sqrt -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Ceil -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Floor -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Exp -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Log_2 -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Log_e -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Log_10 -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Sin -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Cos -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Tan -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Sinh -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Cosh -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Tanh -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Acos -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Asin -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Atan -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.ParseNumber -> ([ E_Type.StringType ], E_Type.StringType)
  | Operators.ParseString -> ([ E_Type.StringType ], E_Type.StringType)
  | Operators.ParseDate -> notImplemented
  | Operators.StringLen -> ([ E_Type.StringType ], E_Type.NumberType)
  | Operators.StringLenU -> ([ E_Type.StringType ], E_Type.NumberType)
  | Operators.Sconcat -> notImplemented
  | Operators.ArrayLen -> notImplemented
  | Operators.ListToArray -> notImplemented
  | Operators.Head -> notImplemented
  | Operators.Tail -> notImplemented
  | Operators.ListLen -> notImplemented
  | Operators.LSort -> notImplemented
  | Operators.LReverse -> notImplemented
  | Operators.LRemoveLast -> notImplemented
  | Operators.First -> notImplemented
  | Operators.Second -> notImplemented
  | Operators.TupleLen -> notImplemented
  | Operators.ObjToList -> notImplemented
  | Operators.ObjFields -> notImplemented

let type_binop (op : Operators.bopt) : E_Type.t list * E_Type.t =
  let notImplemented = ([ E_Type.AnyType; E_Type.AnyType ], E_Type.AnyType) in
  match op with
  | Operators.Plus ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.Minus ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.Times ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.Div ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.Modulo ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.BitwiseAnd ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.BitwiseOr ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.BitwiseXor ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.ShiftLeft ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.ShiftRight ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.ShiftRightLogical ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.Log_And ->
      ([ E_Type.BooleanType; E_Type.BooleanType ], E_Type.BooleanType)
  | Operators.Log_Or ->
      ([ E_Type.BooleanType; E_Type.BooleanType ], E_Type.BooleanType)
  | Operators.Eq -> ([ E_Type.AnyType; E_Type.AnyType ], E_Type.BooleanType)
  | Operators.Lt -> ([ E_Type.AnyType; E_Type.AnyType ], E_Type.BooleanType)
  | Operators.Gt -> ([ E_Type.AnyType; E_Type.AnyType ], E_Type.BooleanType)
  | Operators.Le -> ([ E_Type.AnyType; E_Type.AnyType ], E_Type.BooleanType)
  | Operators.Ge -> ([ E_Type.AnyType; E_Type.AnyType ], E_Type.BooleanType)
  | Operators.Min ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.Max ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.Pow ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.Atan2 ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.IntToBEBytes -> notImplemented
  | Operators.IntFromBytes -> notImplemented
  | Operators.UintFromBytes -> notImplemented
  | Operators.ToPrecision ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.StringType)
  | Operators.ToExponential ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.StringType)
  | Operators.ToFixed ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.StringType)
  | Operators.Snth ->
      ([ E_Type.StringType; E_Type.NumberType ], E_Type.StringType)
  | Operators.Snth_u ->
      ([ E_Type.StringType; E_Type.NumberType ], E_Type.StringType)
  | Operators.Ssplit -> notImplemented
  | Operators.Anth -> notImplemented
  | Operators.ArrayMake -> notImplemented
  | Operators.Lnth -> notImplemented
  | Operators.Ladd -> notImplemented
  | Operators.Lprepend -> notImplemented
  | Operators.Lconcat -> notImplemented
  | Operators.LRem -> notImplemented
  | Operators.LRemNth -> notImplemented
  | Operators.InList -> notImplemented
  | Operators.Tnth -> notImplemented
  | Operators.InObj -> notImplemented

let type_ebinop (op : EOper.bopt) : E_Type.t list * E_Type.t =
  match op with
  | EOper.SCLogAnd ->
      ([ E_Type.BooleanType; E_Type.BooleanType ], E_Type.BooleanType)
  | EOper.SCLogOr ->
      ([ E_Type.BooleanType; E_Type.BooleanType ], E_Type.BooleanType)

let type_triop (op : Operators.topt) : E_Type.t list * E_Type.t =
  let notImplemented =
    ([ E_Type.AnyType; E_Type.AnyType; E_Type.AnyType ], E_Type.AnyType)
  in
  match op with
  | Operators.Ssubstr ->
      ( [ E_Type.StringType; E_Type.NumberType; E_Type.NumberType ],
        E_Type.StringType )
  | Operators.SsubstrU ->
      ( [ E_Type.StringType; E_Type.NumberType; E_Type.NumberType ],
        E_Type.StringType )
  | Operators.Aset -> notImplemented
  | Operators.Lset -> notImplemented
