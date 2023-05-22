let type_unop (op : Operators.uopt) : E_Type.t list * E_Type.t =
  match op with
  | Operators.Neg -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Not -> ([ E_Type.BooleanType ], E_Type.BooleanType)
  | Operators.IsNaN -> ([ E_Type.NumberType ], E_Type.BooleanType)
  | Operators.BitwiseNot -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Typeof -> ([ E_Type.AnyType ], E_Type.TypeType)
  | Operators.IntToFloat -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.IntToString -> ([ E_Type.NumberType ], E_Type.StringType)
  | Operators.IntToFourHex -> ([ E_Type.NumberType ], E_Type.StringType)
  | Operators.IntOfFloat -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.IntOfString -> ([ E_Type.StringType ], E_Type.NumberType)
  | Operators.ToInt -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.ToInt32 -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.ToUint16 -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.ToUint32 -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.OctalToDecimal -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.FloatToString -> ([ E_Type.NumberType ], E_Type.StringType)
  | Operators.FloatOfString -> ([ E_Type.StringType ], E_Type.NumberType)
  (* | Operators.FloatToByte ->  *)
  (* | Operators.Float64ToLEBytes ->  *)
  (* | Operators.Float64ToBEBytes ->  *)
  (* | Operators.Float32ToLEBytes ->  *)
  (* | Operators.Float32ToBEBytes ->  *)
  (* | Operators.Float64FromLEBytes ->  *)
  (* | Operators.Float64FromBEBytes ->  *)
  (* | Operators.Float32FromLEBytes ->  *)
  (* | Operators.Float32FromBEBytes ->  *)
  (* | Operators.BytesToString ->  *)
  | Operators.Utf8Decode -> ([ E_Type.StringType ], E_Type.StringType)
  | Operators.HexDecode -> ([ E_Type.StringType ], E_Type.StringType)
  | Operators.FromCharCode -> ([ E_Type.NumberType ], E_Type.StringType)
  | Operators.FromCharCodeU -> ([ E_Type.NumberType ], E_Type.StringType)
  | Operators.ToCharCode -> ([ E_Type.StringType ], E_Type.NumberType)
  | Operators.ToCharCodeU -> ([ E_Type.StringType ], E_Type.NumberType)
  | Operators.ToLowerCase -> ([ E_Type.StringType ], E_Type.StringType)
  | Operators.ToUpperCase -> ([ E_Type.StringType ], E_Type.StringType)
  | Operators.Trim -> ([ E_Type.StringType ], E_Type.StringType)
  | Operators.Abs -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Acos -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Asin -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Atan -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Ceil -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Cos -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Exp -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Floor -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Log_2 -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Log_e -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Log_10 -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Random -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Sin -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Sqrt -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Tan -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Cosh -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Sinh -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.Tanh -> ([ E_Type.NumberType ], E_Type.NumberType)
  | Operators.ParseNumber -> ([ E_Type.StringType ], E_Type.StringType)
  | Operators.ParseString -> ([ E_Type.StringType ], E_Type.StringType)
  (* | Operators.ParseDate ->  *)
  | Operators.StringLen -> ([ E_Type.StringType ], E_Type.NumberType)
  | Operators.StringLenU -> ([ E_Type.StringType ], E_Type.NumberType)
  (* | Operators.ListLen ->  *)
  (* | Operators.TupleLen ->  *)
  (* | Operators.ArrayLen ->  *)
  (* | Operators.Head ->  *)
  (* | Operators.Tail ->  *)
  (* | Operators.First ->  *)
  (* | Operators.Second ->  *)
  (* | Operators.LRemoveLast ->  *)
  (* | Operators.LSort ->  *)
  (* | Operators.LReverse ->  *)
  (* | Operators.ObjToList ->  *)
  (* | Operators.ObjFields ->  *)
  (* | Operators.ListToArray ->  *)
  (* | Operators.Sconcat ->  *)
  | _ -> ([ E_Type.AnyType ], E_Type.AnyType)

let type_binop (op : Operators.bopt) : E_Type.t list * E_Type.t =
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
  (* | Operators.Eq ->  *)
  (* | Operators.Lt -> *)
  (* | Operators.Gt -> *)
  (* | Operators.Le -> *)
  (* | Operators.Ge -> *)
  | Operators.Min ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.Max ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.Pow ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  | Operators.Atan2 ->
      ([ E_Type.NumberType; E_Type.NumberType ], E_Type.NumberType)
  (* | Operators.IntToBEBytes ->  *)
  (* | Operators.IntFromBytes ->  *)
  (* | Operators.UintFromBytes ->  *)
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
  (* | Operators.Ssplit ->  *)
  (* | Operators.InObj ->  *)
  (* | Operators.InList ->  *)
  (* | Operators.Lnth ->  *)
  (* | Operators.Ladd ->  *)
  (* | Operators.Lprepend ->  *)
  (* | Operators.LRem ->  *)
  (* | Operators.LRemNth ->  *)
  (* | Operators.Lconcat ->  *)
  (* | Operators.Tnth ->  *)
  (* | Operators.Anth ->  *)
  (* | Operators.ArrayMake ->  *)
  | _ -> ([ E_Type.AnyType; E_Type.AnyType ], E_Type.AnyType)

let type_ebinop (op : EOper.bopt) : E_Type.t list * E_Type.t =
  match op with
  | EOper.SCLogAnd ->
      ([ E_Type.BooleanType; E_Type.BooleanType ], E_Type.BooleanType)
  | EOper.SCLogOr ->
      ([ E_Type.BooleanType; E_Type.BooleanType ], E_Type.BooleanType)

let type_triop (op : Operators.topt) : E_Type.t list * E_Type.t =
  match op with
  | Operators.Ssubstr ->
      ( [ E_Type.StringType; E_Type.NumberType; E_Type.NumberType ],
        E_Type.StringType )
  | Operators.SsubstrU ->
      ( [ E_Type.StringType; E_Type.NumberType; E_Type.NumberType ],
        E_Type.StringType )
  (* | Operators.Aset ->  *)
  (* | Operators.Lset ->  *)
  | _ ->
      ([ E_Type.AnyType; E_Type.AnyType; E_Type.AnyType ], E_Type.AnyType)
