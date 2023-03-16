let match_any (texpr : E_Type.t list) : bool = true

let match_all (tref : E_Type.t) (texprs : E_Type.t list) : bool =
  List.for_all (fun texpr -> T_Typing.is_type_compatible tref texpr) texprs

let match_list (trefs : E_Type.t list) (texprs : E_Type.t list) : bool =
  List.for_all
    (fun (tref, texpr) -> T_Typing.is_type_compatible tref texpr)
    (List.combine trefs texprs)

let unop_type (op : Operators.uopt) (texpr : E_Type.t) : E_Type.t option =
  let unop_type' (test : E_Type.t list -> bool) (tres : E_Type.t) :
      E_Type.t option =
    if test [ texpr ] then Some tres else None
  in
  match op with
  | Operators.Neg ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Not ->
      unop_type' (match_list [ E_Type.BooleanType ]) E_Type.BooleanType
  | Operators.IsNaN ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.BooleanType
  | Operators.BitwiseNot ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Typeof -> unop_type' match_any E_Type.TypeType
  | Operators.IntToFloat ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.IntToString ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.StringType
  | Operators.IntToFourHex ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.StringType
  | Operators.IntOfFloat ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.IntOfString ->
      unop_type' (match_list [ E_Type.StringType ]) E_Type.StringType
  | Operators.ToInt ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.ToInt32 ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.ToUint16 ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.ToUint32 ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.OctalToDecimal ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.FloatToString ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.StringType
  | Operators.FloatOfString ->
      unop_type' (match_list [ E_Type.StringType ]) E_Type.NumberType
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
      unop_type' (match_list [ E_Type.StringType ]) E_Type.StringType
  | Operators.HexDecode ->
      unop_type' (match_list [ E_Type.StringType ]) E_Type.StringType
  | Operators.FromCharCode ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.StringType
  | Operators.FromCharCodeU ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.StringType
  | Operators.ToCharCode ->
      unop_type' (match_list [ E_Type.StringType ]) E_Type.NumberType
  | Operators.ToCharCodeU ->
      unop_type' (match_list [ E_Type.StringType ]) E_Type.NumberType
  | Operators.ToLowerCase ->
      unop_type' (match_list [ E_Type.StringType ]) E_Type.StringType
  | Operators.ToUpperCase ->
      unop_type' (match_list [ E_Type.StringType ]) E_Type.StringType
  | Operators.Trim ->
      unop_type' (match_list [ E_Type.StringType ]) E_Type.StringType
  | Operators.Abs ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Acos ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Asin ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Atan ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Ceil ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Cos ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Exp ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Floor ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Log_2 ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Log_e ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Log_10 ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Random ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Sin ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Sqrt ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Tan ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Cosh ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Sinh ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.Tanh ->
      unop_type' (match_list [ E_Type.NumberType ]) E_Type.NumberType
  | Operators.ParseNumber ->
      unop_type' (match_list [ E_Type.StringType ]) E_Type.StringType
  | Operators.ParseString ->
      unop_type' (match_list [ E_Type.StringType ]) E_Type.StringType
  (* | Operators.ParseDate ->  *)
  | Operators.StringLen ->
      unop_type' (match_list [ E_Type.StringType ]) E_Type.NumberType
  | Operators.StringLenU ->
      unop_type' (match_list [ E_Type.StringType ]) E_Type.NumberType
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
  | default -> Some E_Type.AnyType

let binop_type (op : Operators.bopt) (texpr1 : E_Type.t) (texpr2 : E_Type.t) :
    E_Type.t option =
  let binop_type' (test : E_Type.t list -> bool) (tres : E_Type.t) :
      E_Type.t option =
    if test [ texpr1; texpr2 ] then Some tres else None
  in
  match op with
  | Operators.Plus ->
      binop_type' (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Minus ->
      binop_type' (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Times ->
      binop_type' (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Div -> binop_type' (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Modulo ->
      binop_type' (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.BitwiseAnd ->
      binop_type' (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.BitwiseOr ->
      binop_type' (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.BitwiseXor ->
      binop_type' (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.ShiftLeft ->
      binop_type' (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.ShiftRight ->
      binop_type' (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.ShiftRightLogical ->
      binop_type' (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Log_And ->
      binop_type' (match_all E_Type.BooleanType) E_Type.BooleanType
  | Operators.Log_Or ->
      binop_type' (match_all E_Type.BooleanType) E_Type.BooleanType
  (* | Operators.Eq ->  *)
  (* | Operators.Lt -> *)
  (* | Operators.Gt -> *)
  (* | Operators.Le -> *)
  (* | Operators.Ge -> *)
  | Operators.Min -> binop_type' (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Max -> binop_type' (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Pow -> binop_type' (match_all E_Type.NumberType) E_Type.NumberType
  | Operators.Atan2 ->
      binop_type' (match_all E_Type.NumberType) E_Type.NumberType
  (* | Operators.IntToBEBytes ->  *)
  (* | Operators.IntFromBytes ->  *)
  (* | Operators.UintFromBytes ->  *)
  | Operators.ToPrecision ->
      binop_type' (match_all E_Type.NumberType) E_Type.StringType
  | Operators.ToExponential ->
      binop_type' (match_all E_Type.NumberType) E_Type.StringType
  | Operators.ToFixed ->
      binop_type' (match_all E_Type.NumberType) E_Type.StringType
  | Operators.Snth ->
      binop_type'
        (match_list [ E_Type.StringType; E_Type.NumberType ])
        E_Type.StringType
  | Operators.Snth_u ->
      binop_type'
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
  | default -> Some E_Type.AnyType

let ebinop_type (op : EOper.bopt) (texpr1 : E_Type.t) (texpr2 : E_Type.t) :
    E_Type.t option =
  let ebinop_type' (test : E_Type.t list -> bool) (tres : E_Type.t) :
      E_Type.t option =
    if test [ texpr1; texpr2 ] then Some tres else None
  in
  match op with
  | EOper.SCLogAnd ->
      ebinop_type' (match_all E_Type.BooleanType) E_Type.BooleanType
  | EOper.SCLogOr ->
      ebinop_type' (match_all E_Type.BooleanType) E_Type.BooleanType

let triop_type (op : Operators.topt) (texpr1 : E_Type.t) (texpr2 : E_Type.t)
    (texpr3 : E_Type.t) : E_Type.t option =
  let triop_type' (test : E_Type.t list -> bool) (tres : E_Type.t) :
      E_Type.t option =
    if test [ texpr1; texpr2; texpr3 ] then Some tres else None
  in
  match op with
  | Operators.Ssubstr ->
      triop_type'
        (match_list [ E_Type.StringType; E_Type.NumberType; E_Type.NumberType ])
        E_Type.StringType
  | Operators.SsubstrU ->
      triop_type'
        (match_list [ E_Type.StringType; E_Type.NumberType; E_Type.NumberType ])
        E_Type.StringType
  (* | Operators.Aset ->  *)
  (* | Operators.Lset ->  *)
  | default -> Some E_Type.AnyType
