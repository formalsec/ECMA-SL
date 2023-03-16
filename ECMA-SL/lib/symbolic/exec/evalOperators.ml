open Sval

type t = Sval.t

module Op = Operators

let s_len (v : t) : t =
  match v with
  | Str s -> Int (String.length s)
  | Symbolic(Type.StrType, name) -> Unop(Op.StringLen, v)
  | _ ->
      invalid_arg
        "Exception in Oper.s_len: this operation is only applicable to String \
          arguments"


let s_nth ((v1, v2) : t * t) : t =
  match (v1, v2) with
  | Str s, Int i -> Str (String.sub s i 1)
  | Symbolic(Type.StrType, _), Int i-> Binop(Op.Snth, v1, v2)
  | Str s, Symbolic(Type.IntType, _) -> Binop(Op.Snth, v1, v2)
  | _ ->
      invalid_arg
        "Exception in Oper.s_nth: this operation is only applicable to String \
          and Integer arguments"

let eval_unop (op : Op.uopt) (v : t) : t =
  match op with
  | Op.Neg -> (
      match v with
      | Flt f -> Flt (-.f)
      | Int i -> Int (-i)
      | _ -> Unop (Op.Neg, v))
  | Op.Not -> ( match v with Bool b -> Bool (not b) | _ -> Unop (Op.Not, v))
  | Op.Head -> List.hd (to_list v)
  | Op.Tail -> List (List.tl (to_list v))
  | Op.First -> List.hd (to_tuple v)
  | Op.Second -> List.nth (to_tuple v) 1
  | Op.ListLen -> Int (List.length (to_list v))
  | Op.TupleLen -> Int (List.length (to_tuple v))
  | Op.IntToFloat -> Flt (float_of_int (to_int v))
  | Op.IntToString -> Str (string_of_int (to_int v))
  | Op.FloatToString -> Str (string_of_float (to_float v))
  | Op.Typeof -> (
      match v with
      | Int _ -> Type Type.IntType
      | Flt _ -> Type Type.FltType
      | Bool _ -> Type Type.BoolType
      | Str _ -> Type Type.StrType
      | Loc _ -> Type Type.LocType
      | List _ -> Type Type.ListType
      | Arr _ -> Type Type.ArrayType
      | Type _ -> Type Type.TypeType
      | Tuple _ -> Type Type.TupleType
      | Null -> Type Type.NullType
      | Symbol _ -> Type Type.SymbolType
      | Curry _ -> Type Type.CurryType
      | Void -> invalid_arg "typeof: unexpected void value"
      | Symbolic (t, _) -> Type t
      (* FIXME: get type of Sval Binop and Unop *)
      | Unop (_, _) -> Type Type.FltType
      | Binop (_, _, _) -> Type Type.FltType
      | _ ->
          invalid_arg 
            ("typeof(" ^ Sval.str v ^ ") not implemented!"))
  | Op.Sconcat ->
      Str
        (String.concat ""
           (List.fold_left
              (fun a b -> match b with Str s -> a @ [ s ] | _ -> a)
              [] (to_list v)))
  | Op.Trim -> (
      match v with
      | Str s -> Str (String_utils.trim s)
      | _ -> invalid_arg ("Trim: expects Str argument but got " ^ str v))
  | Op.StringLen -> (
      match v with
      | Str s -> Int (String.length s)
      | _ -> invalid_arg ("StringLen: expects Str argument but got " ^ str v))
  | Op.StringLenU -> (
      match v with
      | Str s -> Int (String_utils.s_len_u s)
      | _ -> invalid_arg ("StringLenU: expects Str argument but got " ^ str v))
  | Op.FloatOfString -> (
      match v with
      | Str s -> (
          let trimmed = String.trim s in
          if String.length trimmed == 0 then Flt nan
          else try Flt (float_of_string trimmed) with exn -> Flt nan)
      | _ ->
          invalid_arg
            ("Unop.FloatOfString expects Str argument but got " ^ str v))
  | Op.Exp -> Flt (Float.exp (to_float v))
  | Op.Log_e -> Flt (Float.log (to_float v))
  | Op.Log_10 -> Flt (Float.log10 (to_float v))
  | Op.Sqrt -> Flt (Float.sqrt (to_float v))
  | Op.Abs -> Flt (Float.abs (to_float v))
  | Op.Floor -> Flt (Float.floor (to_float v))
  | Op.ToInt -> Flt (Arith_utils.to_int (to_float v))
  | Op.ToUint32 -> Flt (Arith_utils.to_uint32 (to_float v))
  | Op.IsNaN -> (
      match v with
      | Flt n -> Bool (Float.is_nan n)
      (* Encode to unary op *)
      | v' -> Binop (Op.Eq, v', Flt nan))
  | Op.StringLen -> s_len v
  | _ ->
      invalid_arg ("eval_unop: '" ^ Op.str_of_unopt op ^ "' not implemented")

let eval_binop (op : Op.bopt) (v1 : t) (v2 : t) : t =
  match op with
  | Op.Plus -> (
      match (v1, v2) with
      | Int i1, Int i2 -> Int (i1 + i2)
      | Flt f1, Flt f2 -> Flt (f1 +. f2)
      | _ -> Binop (op, v1, v2))
  | Op.Minus -> (
      match (v1, v2) with
      | Int i1, Int i2 -> Int (i1 - i2)
      | Flt f1, Flt f2 -> Flt (f1 -. f2)
      | _ -> Binop (op, v1, v2))
  | Op.Times -> (
      match (v1, v2) with
      | Int i1, Int i2 -> Int (i1 * i2)
      | Flt f1, Flt f2 -> Flt (f1 *. f2)
      | _ -> Binop (op, v1, v2))
  | Op.Div -> (
      match (v1, v2) with
      | Int i1, Int i2 -> Int (i1 / i2)
      | Flt f1, Flt f2 -> Flt (f1 /. f2)
      | _ -> Binop (op, v1, v2))
  | Op.Modulo -> (
      match (v1, v2) with
      | Flt f1, Flt f2 -> Flt (mod_float f1 f2)
      | _ -> Binop (op, v1, v2))
  | Op.Eq -> (
      match (v1, v2) with
      | Flt f1, Flt f2 -> Bool (Float.equal f1 f2)
      | Arr a1, Arr a2 -> Bool (a1 == a2)
      | v', Null when is_symbolic v' -> Bool false
      | v', Symbol s when not (is_symbol v') -> Bool false
      | _ when (not (is_symbolic v1)) && not (is_symbolic v2) -> Bool (v1 = v2)
      | _ -> Binop (op, v1, v2))
  | Op.Lt -> (
      match (v1, v2) with
      | Int i1, Int i2 -> Bool (i1 < i2)
      | Flt f1, Flt f2 -> Bool (f1 < f2)
      | _ -> Binop (op, v1, v2))
  | Op.Gt -> (
      match (v1, v2) with
      | Int i1, Int i2 -> Bool (i1 > i2)
      | Flt f1, Flt f2 -> Bool (f1 > f2)
      | _ -> Binop (op, v1, v2))
  | Op.Ge -> (
      match (v1, v2) with
      | Int i1, Int i2 -> Bool (i1 >= i2)
      | Flt f1, Flt f2 -> Bool (f1 >= f2)
      | _ -> Binop (op, v1, v2))
  | Op.Le -> (
      match (v1, v2) with
      | Int i1, Int i2 -> Bool (i1 <= i2)
      | Flt f1, Flt f2 -> Bool (f1 <= f2)
      | _ -> Binop (op, v1, v2))
  | Op.Min -> Flt (Float.min (to_float v1) (to_float v2))
  | Op.Max -> Flt (Float.max (to_float v1) (to_float v2))
  | Op.Pow -> Flt (Float.pow (to_float v1) (to_float v2))
  | Op.Log_And -> (
      match (v1, v2) with
      | Bool b1, Bool b2 -> Bool (b1 && b2)
      | _ -> Binop (op, v1, v2))
  | Op.Log_Or -> (
      match (v1, v2) with
      | Bool b1, Bool b2 -> Bool (b1 || b2)
      | _ -> Binop (op, v1, v2))
  | Op.Tnth -> (
      match (v1, v2) with
      | Tuple l, Int i -> List.nth l i
      | _ ->
          invalid_arg
            "Op.t_nth: operation only applicable to Tuple and Int arguments")
  | Op.Lnth -> (
      match (v1, v2) with
      | List l, Int i -> List.nth l i
      | _ ->
          invalid_arg
            "Op.l_nth: operation only applicable to List and Int arguments")
  | Op.InList -> (
      match v2 with
      | List l -> Bool (List.mem v1 l)
      | _ ->
          invalid_arg "Op.list_in: operation only applicable to List arguments")
  | Op.Lprepend -> (
      match v2 with
      | List l -> List (v1 :: l)
      | _ ->
          invalid_arg "Op.Lprepend: operation only applicable to List arguments"
      )
  | Op.Ladd -> (
      match v1 with
      | List l -> List (l @ [ v2 ])
      | _ -> invalid_arg "Op.Ladd: operation only applicable to List arguments")
  | Op.Pow -> Flt (Float.pow (to_float v1) (to_float v2))
  | Op.Snth -> s_nth(v1, v2)
  | _ ->
      invalid_arg
        ("eval_binop: " ^ Op.str_of_binopt_single op ^ " not implemented!")

let eval_triop (op : Op.topt) (v1 : t) (v2 : t) (v1 : t) : t =
  invalid_arg "eval_triop: not implemented"

let eval_nop (op : Op.nopt) (vs : t list) : t =
  match op with
  | Op.ListExpr -> List vs
  | Op.TupleExpr -> Tuple vs
  | _ ->
      invalid_arg ("eval_nop: " ^ Op.str_of_nopt op [ "" ] ^ " not implemented")
