open Smtml
open EslSyntax
open EslSyntax.Operator

let op_err (arg : int) (op_lbl : string) (rterr : Runtime_error.msg) : 'a =
  try Runtime_error.(throw ~src:(Index arg) rterr)
  with Runtime_error.Error err ->
    Runtime_error.(push (OpEvalErr op_lbl) err |> raise)

let unexpected_err (arg : int) (op_lbl : string) (msg : string) : 'a =
  op_err arg op_lbl (Unexpected msg)

let bad_arg_err (arg : int) (op_lbl : string) (types : string)
  (vals : Value.t list) : 'a =
  op_err arg op_lbl (BadOpArgs (types, vals))


let unop_semantics (op : Operator.unopt) = 
  match op with
  | Typeof -> (* TODO: *) failwith "unop_semantics.typeof"
  | Neg -> (function
            | Value.Int _ as v-> Eval.(unop Ty_int Ty.Neg v)
            | Value.Real _ as v-> Eval.(unop Ty_real Ty.Neg v)
            | _ -> failwith "unop_semantics.neg")
  | BitwiseNot -> (function 
            | Value.Int _ as v-> Eval.(unop Ty_int Ty.Not v)
            | _ -> failwith "unop_semantics.bitwiseNot")
  | LogicalNot -> (function 
            | (Value.True | Value.False) as v -> Eval.(unop Ty_bool Ty.Not v)
            | _ -> failwith "unop_semantics.logicalNot")
  | IntToFloat -> (function 
            | Value.Int _ as v -> Eval.(cvtop Ty_int Ty.Reinterpret_int v)
            | _ -> failwith "unop_semantics.intToFloat")
  | IntToString -> (function
            | Value.Int _ as v -> Eval.(cvtop Ty_str Ty.String_from_int v)
            | _ -> failwith "unop_semantics.intToString")
  | FloatToInt -> (function
            | Value.Real _ as v -> Eval.(cvtop Ty_real Ty.Reinterpret_float v)
            | _ -> failwith "unop_semantics.floatToInt")
  | FloatToString -> (function
            | Value.Real _ as v -> Eval.(cvtop Ty_real Ty.ToString v)
            | _ -> failwith "unop_semantics.floatToString")
  | StringToInt -> (function
            | Value.Str _ as v -> Eval.(cvtop Ty_str Ty.String_to_int v)
            | _ -> failwith "unop_semantics.stringToInt")
  | StringToFloat -> (function
            | Value.Str _ as v -> Eval.(cvtop Ty_str Ty.String_to_float v)
            | _ -> failwith "unop_semantics.stringToFloat")
  | FromCharCode -> (function
            | Value.Int _ as v -> Eval.(cvtop Ty_str Ty.String_from_code v)
            | _ -> failwith "unop_semantics.fromCharCode")
  | ToCharCode -> (function
            | Value.Str _ as v -> Eval.(cvtop Ty_str Ty.String_to_code v)
            | _ -> failwith "unop_semantics.toCharCode")
  | StringLen -> (function
            | Value.Str _ as v -> Eval.(unop Ty_str Ty.Length v)
            | _ -> failwith "unop_semantics.stringLen")
  (* TODO: tocheck StringConcat *)
  | StringConcat -> (* (function
            | Value.List _ as v -> Eval.(unop Ty_list Ty.Concat v)
            | _ -> failwith "unop_semantics.stringConcat") *)
            assert false
  | ObjectToList -> failwith "unop_semantics.objectToList"
  | ObjectFields -> failwith "unop_semantics.objectFields"
  | ListHead | TupleFirst -> (function
            | Value.List _ as v -> Eval.(unop Ty_list Ty.Head v)
            | _ -> failwith "unop_semantics.listHead/tupleFirst")
  | ListTail | TupleSecond -> (function
            | Value.List _ as v -> Eval.(unop Ty_list Ty.Tail v)
            | _ -> failwith "unop_semantics.listTail/tupleSecond")
  | ListLen | TupleLen -> (function
            | Value.List _ as v -> Eval.(unop Ty_list Ty.Length v)
            | _ -> failwith "unop_semantics.listLen/tupleLen")
  | ListReverse -> (function
            | Value.List _ as v -> Eval.(unop Ty_list Ty.Reverse v)
            | _ -> failwith "unop_semantics.listReverse") 
  | Random -> (* TODO:x check external function or keep? *) failwith "unop_semantics.random"
  | Abs -> (function
            | Value.Real _ as v -> Eval.(unop Ty_real Ty.Abs v)
            | _ -> failwith "unop_semantics.abs")
  | Sqrt -> (function
            | Value.Real _ as v -> Eval.(unop Ty_real Ty.Sqrt v)
            | _ -> failwith "unop_semantics.sqrt")
  | Ceil -> (function
            | Value.Real _ as v -> Eval.(unop Ty_real Ty.Ceil v)
            | _ -> failwith "unop_semantics.ceil")
  | Floor -> (function
            | Value.Real _ as v -> Eval.(unop Ty_real Ty.Floor v)
            | _ -> failwith "unop_semantics.floor")
  | Trunc -> (function
            | Value.Real _ as v -> Eval.(unop Ty_real Ty.Trunc v)
            | _ -> failwith "unop_semantics.trunc")
  | Exp -> (* TODO:x check external function or keep? *) failwith "unop_semantics.exp"
  
let binop_semantics (op : Operator.binopt) =
  let make_bool b = if b then Value.True else Value.False in
  match op with
  | Plus -> (fun v1 v2 -> match v1, v2 with 
            | Value.Int _ , Value.Int _  -> Eval.(binop Ty_int Ty.Add v1 v2)
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Add v1 v2)
            | Value.Str _ , Value.Str _  -> Eval.(naryop Ty_str Ty.Concat [v1; v2])
            | _ -> failwith "binop_semantics.plus")
  | Minus -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop Ty_int Ty.Sub v1 v2)
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Sub v1 v2)
            | _ -> failwith "binop_semantics.minus")
  | Times -> (fun v1 v2 -> match v1, v2 with 
            | Value.Int _ , Value.Int _  -> Eval.(binop Ty_int Ty.Mul v1 v2)
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Mul v1 v2)
            | _ -> failwith "binop_semantics.times")
  | Div -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop Ty_int Ty.Div v1 v2)
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Div v1 v2)
            | _ -> failwith "binop_semantics.div")
  | Modulo -> (fun v1 v2 -> match v1, v2 with
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Rem v1 v2)
            | _ -> failwith "binop_semantics.modulo")
  | Pow -> (fun v1 v2 -> match v1, v2 with
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Pow v1 v2)
            | _ -> failwith "binop_semantics.pow")
  (* FIXME: check if bitwise operators are correct (float vs int) *)
  | BitwiseAnd -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop (Ty_bitv 32) Ty.And v1 v2)
            | _ -> failwith "binop_semantics.bitwiseAnd")
  | BitwiseOr -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop (Ty_bitv 32) Ty.Or v1 v2)
            | _ -> failwith "binop_semantics.bitwiseOr")
  | BitwiseXor -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop (Ty_bitv 32) Ty.Xor v1 v2)
            | _ -> failwith "binop_semantics.bitwiseXor")
  | ShiftLeft -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop (Ty_bitv 32)  Ty.Shl v1 v2)
            | _ -> failwith "binop_semantics.shiftLeft")
  | ShiftRight -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop (Ty_bitv 32)  Ty.ShrA v1 v2)
            | _ -> failwith "binop_semantics.shiftRight")
  | ShiftRightLogical -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop (Ty_bitv 32)  Ty.ShrL v1 v2)
            | _ -> failwith "binop_semantics.shiftRightExt")
  | LogicalAnd -> (fun v1 v2 -> match v1, v2 with
            | (Value.True | Value.False), (Value.True | Value.False) -> Eval.(binop Ty_bool Ty.And v1 v2)
            | _ -> failwith "binop_semantics.logicalAnd")
  | LogicalOr -> (fun v1 v2 -> match v1, v2 with
            | (Value.True | Value.False), (Value.True | Value.False) -> Eval.(binop Ty_bool Ty.Or v1 v2)
            | _ -> failwith "binop_semantics.logicalOr")
  | SCLogicalAnd -> failwith "binop_semantics.scLogicalAnd"
  | SCLogicalOr -> failwith "binop_semantics.scLogicalOr"
  | Eq -> (fun v1 v2 -> match v1, v2 with
            | (Value.True | Value.False), (Value.True | Value.False) -> make_bool Eval.(relop Ty_bool Ty.Eq v1 v2)
            | _ -> failwith "binop_semantics.eq")
  | NE -> (fun v1 v2 -> match v1, v2 with
            | (Value.True | Value.False), (Value.True | Value.False) -> make_bool Eval.(relop Ty_bool Ty.Ne v1 v2)
            | _ -> failwith "binop_semantics.ne")
  | Lt -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> make_bool Eval.(relop Ty_int Ty.Lt v1 v2)
            | Value.Real _ , Value.Real _  -> make_bool Eval.(relop Ty_real Ty.Lt v1 v2)
            | _ -> failwith "binop_semantics.lt")
  | Le -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> make_bool Eval.(relop Ty_int Ty.Le v1 v2) 
            | Value.Real _ , Value.Real _  -> make_bool Eval.(relop Ty_real Ty.Le v1 v2)
            | _ -> failwith "binop_semantics.le")
  | Gt -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> make_bool Eval.(relop Ty_int Ty.Gt v1 v2)
            | Value.Real _ , Value.Real _  -> make_bool Eval.(relop Ty_real Ty.Gt v1 v2)
            | _ -> failwith "binop_semantics.gt")
  | Ge -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> make_bool Eval.(relop Ty_int Ty.Ge v1 v2)
            | Value.Real _ , Value.Real _  -> make_bool Eval.(relop Ty_real Ty.Ge v1 v2)
            | _ -> failwith "binop_semantics.ge")
  | ObjectMem -> failwith "binop_semantics.objectMem"
  | StringNth -> (fun v1 v2 -> match v1, v2 with
            | Value.Str _, Value.Int _ -> Eval.(binop Ty_str Ty.At v1 v2)
            | _ -> failwith "binop_semantics.stringNth")
  | ListNth | TupleNth -> (fun v1 v2 -> match v1, v2 with
            | Value.List _, Value.Int _ -> Eval.(binop Ty_list Ty.At v1 v2)
            | _ -> failwith "binop_semantics.listNth/tupleNth")
  | ListAdd -> (fun v1 v2 -> match v1, v2 with
            | Value.List _, Value.List _ -> Eval.(binop Ty_list Ty.List_append_last v1 v2)
            | _ -> failwith "binop_semantics.ListAdd")
  | ListPrepend -> (fun v1 v2 -> match v1, v2 with
            | Value.List _, _ -> Eval.(binop Ty_list Ty.List_append v1 v2)
            | _ -> failwith "binop_semantics.ListPrepend")
  | ListConcat -> (fun v1 v2 -> match v1, v2 with
            | Value.List _, Value.List _ -> Eval.(naryop Ty_list Ty.Concat [v1;v2])
            | _ -> failwith "binop_semantics.ListConcat")

let triop_semantics (op: Operator.triopt) =
  match op with
  | ITE -> (fun v1 v2 v3 -> Eval.triop Ty_bool Ty.Ite v1 v2 v3)
  | StringSubstr -> (fun v1 v2 v3 -> match v1, v2, v3 with
            | Value.Str _, Value.Int _, Value.Int _ -> Eval.(triop Ty_str Ty.String_extract v1 v2 v3)
            | _ -> failwith "triopt_semantics.stringSubstr")
  | ListSet -> (fun v1 v2 v3 -> match v1, v2, v3 with
            | Value.List _, Value.Int _, _ -> Eval.(triop Ty_list Ty.List_set v1 v2 v3)
            | _ -> failwith "triopt_semantics.listSet")

let to_bool_aux (v : Value.t) : bool =
  match v with 
  | Value.True -> true
  | Value.False -> false
  | _ -> failwith "Eval_op.to_bool_aux"

let nary_logical_and (vals : Value.t list) : Value.t =
  if (List.for_all to_bool_aux vals) then
    Value.True
  else 
    Value.False

let nary_logical_or (vals : Value.t list) : Value.t =
  if (List.exists to_bool_aux vals) then 
    Value.True
  else 
    Value.False

let eval_nopt (op : nopt) (vals : Value.t list) : Value.t =
  match op with
  | NAryLogicalAnd -> nary_logical_and vals
  | NAryLogicalOr -> nary_logical_or vals
  | ArrayExpr -> (* TODO:x Value.Arr (Array.of_list vals) *) failwith "eval_nopt.arrayExpr"
  | ListExpr -> Value.List vals
  | TupleExpr -> Value.List vals