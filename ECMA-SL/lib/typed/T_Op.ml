let match_any (texprs : E_Type.t list) : bool = true

let match_all (tref : E_Type.t) (texprs : E_Type.t list) : bool =
  List.for_all (fun texpr -> T_Typing.is_typeable tref texpr) texprs

let match_list (trefs : E_Type.t list) (texprs : E_Type.t list) : bool =
  List.for_all
    (fun (tref, texpr) -> T_Typing.is_typeable tref texpr)
    (List.combine trefs texprs)

let test_op (test_fun : E_Type.t list -> bool) (tres : E_Type.t)
    (exprs : E_Type.t list) : E_Type.t option =
  if test_fun exprs then Some tres else None

let unop_typing_fun (op : Operators.uopt) : E_Type.t list -> E_Type.t option =
  match op with
  | Operators.Neg -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Not -> test_op (match_all E_Type.BooleanType) E_Type.BooleanType
  | Operators.IsNaN -> test_op (match_all E_Type.NumberType) E_Type.BooleanType
  | Operators.BitwiseNot ->
      test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Typeof -> test_op match_any E_Type.TypeType
  | Operators.IntToFloat ->
      test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.IntToString ->
      test_op (match_all E_Type.NumberType) E_Type.StringType
  | Operators.IntToFourHex ->
      test_op (match_all E_Type.NumberType) E_Type.StringType
  | Operators.IntOfFloat ->
      test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.IntOfString ->
      test_op (match_all E_Type.StringType) E_Type.StringType
  | Operators.ToInt -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.ToInt32 -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.ToUint16 ->
      test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.ToUint32 ->
      test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.OctalToDecimal ->
      test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.FloatToString ->
      test_op (match_all E_Type.NumberType) E_Type.StringType
  | Operators.FloatOfString ->
      test_op (match_all E_Type.StringType) E_Type.NumberType
  (* | Operators.FloatToByte -> *)
  (* | Operators.Float64ToLEBytes ->  *)
  (* | Operators.Float64ToBEBytes ->  *)
  (* | Operators.Float32ToLEBytes ->  *)
  (* | Operators.Float32ToBEBytes ->  *)
  (* | Operators.Float64FromLEBytes ->  *)
  (* | Operators.Float64FromBEBytes ->  *)
  (* | Operators.Float32FromLEBytes ->  *)
  (* | Operators.Float32FromBEBytes ->  *)
  (* | Operators.BytesToString ->  *)
  | Operators.Utf8Decode ->
      test_op (match_all E_Type.StringType) E_Type.StringType
  | Operators.HexDecode ->
      test_op (match_all E_Type.StringType) E_Type.StringType
  | Operators.FromCharCode ->
      test_op (match_all E_Type.NumberType) E_Type.StringType
  | Operators.FromCharCodeU ->
      test_op (match_all E_Type.NumberType) E_Type.StringType
  | Operators.ToCharCode ->
      test_op (match_all E_Type.StringType) E_Type.NumberType
  | Operators.ToCharCodeU ->
      test_op (match_all E_Type.StringType) E_Type.NumberType
  | Operators.ToLowerCase ->
      test_op (match_all E_Type.StringType) E_Type.StringType
  | Operators.ToUpperCase ->
      test_op (match_all E_Type.StringType) E_Type.StringType
  | Operators.Trim -> test_op (match_all E_Type.StringType) E_Type.StringType
  | Operators.Abs -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Acos -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Asin -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Atan -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Ceil -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Cos -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Exp -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Floor -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Log_2 -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Log_e -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Log_10 -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Random -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Sin -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Sqrt -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Tan -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Cosh -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Sinh -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Tanh -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.ParseNumber ->
      test_op (match_all E_Type.StringType) E_Type.StringType
  | Operators.ParseString ->
      test_op (match_all E_Type.StringType) E_Type.StringType
  (* | Operators.ParseDate ->  *)
  | Operators.StringLen ->
      test_op (match_all E_Type.StringType) E_Type.NumberType
  | Operators.StringLenU ->
      test_op (match_all E_Type.StringType) E_Type.NumberType
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
  | default -> test_op match_any E_Type.AnyType

let binop_typing_fun (op : Operators.bopt) : E_Type.t list -> E_Type.t option =
  match op with
  | Operators.Plus -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Minus -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Times -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Div -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Modulo -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.BitwiseAnd ->
      test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.BitwiseOr ->
      test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.BitwiseXor ->
      test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.ShiftLeft ->
      test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.ShiftRight ->
      test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.ShiftRightLogical ->
      test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Log_And ->
      test_op (match_all E_Type.BooleanType) E_Type.BooleanType
  | Operators.Log_Or ->
      test_op (match_all E_Type.BooleanType) E_Type.BooleanType
  (* | Operators.Eq ->  *)
  (* | Operators.Lt -> *)
  (* | Operators.Gt -> *)
  (* | Operators.Le -> *)
  (* | Operators.Ge -> *)
  | Operators.Min -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Max -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Pow -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Atan2 -> test_op (match_all E_Type.NumberType) E_Type.NumberType
  (* | Operators.IntToBEBytes ->  *)
  (* | Operators.IntFromBytes ->  *)
  (* | Operators.UintFromBytes ->  *)
  | Operators.ToPrecision ->
      test_op (match_all E_Type.NumberType) E_Type.StringType
  | Operators.ToExponential ->
      test_op (match_all E_Type.NumberType) E_Type.StringType
  | Operators.ToFixed -> test_op (match_all E_Type.NumberType) E_Type.StringType
  | Operators.Snth ->
      test_op
        (match_list [ E_Type.StringType; E_Type.NumberType ])
        E_Type.StringType
  | Operators.Snth_u ->
      test_op
        (match_list [ E_Type.StringType; E_Type.NumberType ])
        E_Type.StringType
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
  | default -> test_op match_any E_Type.AnyType

let ebinop_typing_fun (op : EOper.bopt) : E_Type.t list -> E_Type.t option =
  match op with
  | EOper.SCLogAnd -> test_op (match_all E_Type.BooleanType) E_Type.BooleanType
  | EOper.SCLogOr -> test_op (match_all E_Type.BooleanType) E_Type.BooleanType

let triop_typing_fun (op : Operators.topt) : E_Type.t list -> E_Type.t option =
  match op with
  | Operators.Ssubstr ->
      test_op
        (match_list [ E_Type.StringType; E_Type.NumberType; E_Type.NumberType ])
        E_Type.StringType
  | Operators.SsubstrU ->
      test_op
        (match_list [ E_Type.StringType; E_Type.NumberType; E_Type.NumberType ])
        E_Type.StringType
  (* | Operators.Aset ->  *)
  (* | Operators.Lset ->  *)
  | default -> test_op match_any E_Type.AnyType

let type_op (typing_fun : E_Type.t list -> E_Type.t option)
    (texprs : E_Type.t list) : E_Type.t option =
  typing_fun texprs
