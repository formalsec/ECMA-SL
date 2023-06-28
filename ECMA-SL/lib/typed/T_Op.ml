type funcPrototype_t = E_Type.t list * E_Type.t

let type_unop (op : Operators.uopt) : funcPrototype_t list =
  let notImplemented = [ ([ E_Type.AnyType ], E_Type.AnyType) ] in
  match op with
  | Operators.Neg ->
      [
        ([ E_Type.IntType ], E_Type.IntType);
        ([ E_Type.FloatType ], E_Type.FloatType);
      ]
  | Operators.Not -> [ ([ E_Type.BooleanType ], E_Type.BooleanType) ]
  | Operators.BitwiseNot -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.IsNaN -> [ ([ E_Type.AnyType ], E_Type.BooleanType) ]
  | Operators.Typeof ->
      [ ([ E_Type.AnyType ], E_Type.RuntimeType Type.TypeType) ]
  | Operators.ToInt -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.ToInt32 -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.ToUint16 -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.ToUint32 -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.IntToFloat -> [ ([ E_Type.IntType ], E_Type.FloatType) ]
  | Operators.IntOfFloat -> [ ([ E_Type.FloatType ], E_Type.IntType) ]
  | Operators.IntToString -> [ ([ E_Type.IntType ], E_Type.StringType) ]
  | Operators.IntOfString -> [ ([ E_Type.StringType ], E_Type.IntType) ]
  | Operators.IntToFourHex -> [ ([ E_Type.IntType ], E_Type.StringType) ]
  | Operators.OctalToDecimal -> [ ([ E_Type.IntType ], E_Type.IntType) ]
  | Operators.FloatToString -> [ ([ E_Type.FloatType ], E_Type.StringType) ]
  | Operators.FloatOfString -> [ ([ E_Type.StringType ], E_Type.FloatType) ]
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
  | Operators.Utf8Decode -> [ ([ E_Type.StringType ], E_Type.StringType) ]
  | Operators.HexDecode -> [ ([ E_Type.StringType ], E_Type.StringType) ]
  | Operators.FromCharCode -> [ ([ E_Type.IntType ], E_Type.StringType) ]
  | Operators.FromCharCodeU -> [ ([ E_Type.IntType ], E_Type.StringType) ]
  | Operators.ToCharCode -> [ ([ E_Type.StringType ], E_Type.IntType) ]
  | Operators.ToCharCodeU -> [ ([ E_Type.StringType ], E_Type.IntType) ]
  | Operators.ToLowerCase -> [ ([ E_Type.StringType ], E_Type.StringType) ]
  | Operators.ToUpperCase -> [ ([ E_Type.StringType ], E_Type.StringType) ]
  | Operators.Trim -> [ ([ E_Type.StringType ], E_Type.FloatType) ]
  | Operators.Random -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Abs -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Sqrt -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Ceil -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Floor -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Exp -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Log_2 -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Log_e -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Log_10 -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Sin -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Cos -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Tan -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Sinh -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Cosh -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Tanh -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Acos -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Asin -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Atan -> [ ([ E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.ParseNumber -> [ ([ E_Type.StringType ], E_Type.StringType) ]
  | Operators.ParseString -> [ ([ E_Type.StringType ], E_Type.StringType) ]
  | Operators.ParseDate -> notImplemented
  | Operators.StringLen -> [ ([ E_Type.StringType ], E_Type.IntType) ]
  | Operators.StringLenU -> [ ([ E_Type.StringType ], E_Type.IntType) ]
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

let type_binop (op : Operators.bopt) : funcPrototype_t list =
  let notImplemented =
    [ ([ E_Type.AnyType; E_Type.AnyType ], E_Type.AnyType) ]
  in
  match op with
  | Operators.Plus ->
      [
        ([ E_Type.IntType; E_Type.IntType ], E_Type.IntType);
        ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType);
      ]
  | Operators.Minus ->
      [
        ([ E_Type.IntType; E_Type.IntType ], E_Type.IntType);
        ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType);
      ]
  | Operators.Times ->
      [
        ([ E_Type.IntType; E_Type.IntType ], E_Type.IntType);
        ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType);
      ]
  | Operators.Div ->
      [
        ([ E_Type.IntType; E_Type.IntType ], E_Type.IntType);
        ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType);
      ]
  | Operators.Modulo ->
      [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.BitwiseAnd ->
      [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.BitwiseOr ->
      [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.BitwiseXor ->
      [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.ShiftLeft ->
      [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.ShiftRight ->
      [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.ShiftRightLogical ->
      [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Log_And ->
      [ ([ E_Type.BooleanType; E_Type.BooleanType ], E_Type.BooleanType) ]
  | Operators.Log_Or ->
      [ ([ E_Type.BooleanType; E_Type.BooleanType ], E_Type.BooleanType) ]
  | Operators.Eq -> [ ([ E_Type.AnyType; E_Type.AnyType ], E_Type.BooleanType) ]
  | Operators.Lt -> [ ([ E_Type.AnyType; E_Type.AnyType ], E_Type.BooleanType) ]
  | Operators.Gt -> [ ([ E_Type.AnyType; E_Type.AnyType ], E_Type.BooleanType) ]
  | Operators.Le -> [ ([ E_Type.AnyType; E_Type.AnyType ], E_Type.BooleanType) ]
  | Operators.Ge -> [ ([ E_Type.AnyType; E_Type.AnyType ], E_Type.BooleanType) ]
  | Operators.Min ->
      [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Max ->
      [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Pow ->
      [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.Atan2 ->
      [ ([ E_Type.FloatType; E_Type.FloatType ], E_Type.FloatType) ]
  | Operators.IntToBEBytes -> notImplemented
  | Operators.IntFromBytes -> notImplemented
  | Operators.UintFromBytes -> notImplemented
  | Operators.ToPrecision ->
      [ ([ E_Type.FloatType; E_Type.IntType ], E_Type.StringType) ]
  | Operators.ToExponential ->
      [ ([ E_Type.FloatType; E_Type.IntType ], E_Type.StringType) ]
  | Operators.ToFixed ->
      [ ([ E_Type.FloatType; E_Type.IntType ], E_Type.StringType) ]
  | Operators.Snth ->
      [ ([ E_Type.StringType; E_Type.IntType ], E_Type.StringType) ]
  | Operators.Snth_u ->
      [ ([ E_Type.StringType; E_Type.IntType ], E_Type.StringType) ]
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

let type_ebinop (op : EOper.bopt) : funcPrototype_t list =
  match op with
  | EOper.SCLogAnd ->
      [ ([ E_Type.BooleanType; E_Type.BooleanType ], E_Type.BooleanType) ]
  | EOper.SCLogOr ->
      [ ([ E_Type.BooleanType; E_Type.BooleanType ], E_Type.BooleanType) ]

let type_triop (op : Operators.topt) : funcPrototype_t list =
  let notImplemented =
    [ ([ E_Type.AnyType; E_Type.AnyType; E_Type.AnyType ], E_Type.AnyType) ]
  in
  match op with
  | Operators.Ssubstr ->
      [
        ( [ E_Type.StringType; E_Type.IntType; E_Type.IntType ],
          E_Type.StringType );
      ]
  | Operators.SsubstrU ->
      [
        ( [ E_Type.StringType; E_Type.IntType; E_Type.IntType ],
          E_Type.StringType );
      ]
  | Operators.Aset -> notImplemented
  | Operators.Lset -> notImplemented
