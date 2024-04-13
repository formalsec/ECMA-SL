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
  | Typeof -> (* TODO:x *) failwith "unop_semantics.typeof"
  | Neg -> (function
            | Value.Int _ as v -> Eval.(unop Ty_int Ty.Neg v)
            | Value.Real _ as v -> Eval.(unop Ty_real Ty.Neg v)
            | _ as v -> bad_arg_err 1 "unop_semantics.neg" "integer or float" [ v ])
  | BitwiseNot -> (function 
            | Value.Int _ as v -> Eval.(unop Ty_int Ty.Not v)
            | _ as v -> bad_arg_err 1 "unop_semantics.bitwiseNot" "int" [ v ])
  | LogicalNot -> (function 
            | (Value.True | Value.False) as v -> Eval.(unop Ty_bool Ty.Not v)
            | _ as v-> bad_arg_err 1 "unop_semantics.logicalNot" "boolean" [ v ])
  | IntToFloat -> (function 
            | Value.Int _ as v -> Eval.(cvtop Ty_int Ty.Reinterpret_int v)
            | _ as v -> bad_arg_err 1 "unop_semantics.intToFloat" "integer" [ v ])
  | IntToString -> (function
            | Value.Int _ as v -> Eval.(cvtop Ty_str Ty.String_from_int v)
            | _ as v -> bad_arg_err 1 "unop_semantics.intToString" "integer" [ v ])
  | FloatToInt -> (function
            | Value.Real _ as v -> Eval.(cvtop Ty_real Ty.Reinterpret_float v)
            | _ as v -> bad_arg_err 1 "unop_semantics.floatToInt" "float" [ v ])
  | FloatToString -> (function
            | Value.Real _ as v -> Eval.(cvtop Ty_real Ty.ToString v)
            | _ as v -> bad_arg_err 1 "unop_semantics.floatToString" "float" [ v ])
  | StringToInt -> (function
            | Value.Str _ as v -> Eval.(cvtop Ty_str Ty.String_to_int v)
            | _ as v -> bad_arg_err 1 "unop_semantics.stringToInt" "string" [ v ])
  | StringToFloat -> (function
            | Value.Str _ as v -> Eval.(cvtop Ty_str Ty.String_to_float v)
            | _ as v -> bad_arg_err 1 "unop_semantics.stringToFloat" "string" [ v ])
  | FromCharCode -> (function
            | Value.Int _ as v -> Eval.(cvtop Ty_str Ty.String_from_code v)
            | _ as v -> bad_arg_err 1 "unop_semantics.fromCharCode" "integer" [ v ])
  | ToCharCode -> (function
            | Value.Str _ as v -> Eval.(cvtop Ty_str Ty.String_to_code v)
            | _ as v -> bad_arg_err 1 "unop_semantics.toCharCode" "string" [ v ])
  | StringLen -> (function
            | Value.Str _ as v -> Eval.(unop Ty_str Ty.Length v)
            | _ as v -> bad_arg_err 1 "unop_semantics.stringLen" "string" [ v ])
  (* TODO:x tocheck StringConcat *)
  | StringConcat -> (* (function
            | Value.List _ as v -> Eval.(unop Ty_list Ty.Concat v)
            | _ -> failwith "unop_semantics.stringConcat") *)
            assert false
  | ObjectToList -> failwith "unop_semantics.objectToList"
  | ObjectFields -> failwith "unop_semantics.objectFields"
  | ListHead | TupleFirst -> (function
            | Value.List _ as v -> Eval.(unop Ty_list Ty.Head v)
            | _ as v -> bad_arg_err 1 "unop_semantics.listHead/tupleFirst" "non-empty list/tuple" [ v ])
  (* FIXME: Problems with TupleSecond when using List, because it returns an list instead of the element *)
            | ListTail | TupleSecond -> (function
            | Value.List _ as v -> Eval.(unop Ty_list Ty.Tail v)
            | _ as v -> bad_arg_err 1 "unop_semantics.listTail/tupleSecond" "list/tuple" [ v ])
  | ListLen | TupleLen -> (function
            | Value.List _ as v -> Eval.(unop Ty_list Ty.Length v)
            | _ as v -> bad_arg_err 1 "unop_semantics.listLen/tupleLen" "list/tuple" [ v ])
  | ListReverse -> (function
            | Value.List _ as v -> Eval.(unop Ty_list Ty.Reverse v)
            | _ as v -> bad_arg_err 1 "unop_semantics.listReverse" "list" [ v ])
  | Random -> (* TODO:x check external function or keep? *) failwith "unop_semantics.random"
  | Abs -> (function
            | Value.Real _ as v -> Eval.(unop Ty_real Ty.Abs v)
            | _ as v -> bad_arg_err 1 "unop_semantics.abs" "float" [ v ])
  | Sqrt -> (function
            | Value.Real _ as v -> Eval.(unop Ty_real Ty.Sqrt v)
            | _ as v -> bad_arg_err 1 "unop_semantics.sqrt" "float" [ v ])
  | Ceil -> (function
            | Value.Real _ as v -> Eval.(unop Ty_real Ty.Ceil v)
            | _ as v -> bad_arg_err 1 "unop_semantics.ceil" "float" [ v ])
  | Floor -> (function
            | Value.Real _ as v -> Eval.(unop Ty_real Ty.Floor v)
            | _ as v -> bad_arg_err 1 "unop_semantics.floor" "float" [ v ])
  | Trunc -> (function
            | Value.Real _ as v -> Eval.(unop Ty_real Ty.Trunc v)
            | _ as v -> bad_arg_err 1 "unop_semantics.trunc" "float" [ v ])
  | Exp -> (* TODO:x check external function or keep? *) failwith "unop_semantics.exp"
  
let binop_semantics (op : Operator.binopt) =
  let make_bool b = if b then Value.True else Value.False in
  match op with
  | Plus -> (fun v1 v2 -> match v1, v2 with 
            | Value.Int _ , Value.Int _  -> Eval.(binop Ty_int Ty.Add v1 v2)
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Add v1 v2)
            | Value.Str _ , Value.Str _  -> Eval.(naryop Ty_str Ty.Concat [v1; v2])
            | ((Int _ | Real _ | Str _), _) -> bad_arg_err 2 "binop_semantics.plus"
            "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.plus"
            "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ])
  | Minus -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop Ty_int Ty.Sub v1 v2)
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Sub v1 v2)
            | (Int _, _) | (Real _, _) ->
              bad_arg_err 2 "binop_semantics.minus" "(integer, integer) or (float, float)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.minus" "(integer, integer) or (float, float)" [ v1; v2 ])
  | Times -> (fun v1 v2 -> match v1, v2 with 
            | Value.Int _ , Value.Int _  -> Eval.(binop Ty_int Ty.Mul v1 v2)
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Mul v1 v2)
            | (Int _, _) | (Real _, _) ->
              bad_arg_err 2 "binop_semantics.times" "(integer, integer) or (float, float)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.times" "(integer, integer) or (float, float)" [ v1; v2 ])
  | Div -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop Ty_int Ty.Div v1 v2)
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Div v1 v2)
            | (Int _, _) | (Real _, _) ->
              bad_arg_err 2 "binop_semantics.div" "(integer, integer) or (float, float)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.div" "(integer, integer) or (float, float)" [ v1; v2 ])
  | Modulo -> (fun v1 v2 -> match v1, v2 with
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Rem v1 v2)
            | (Real _, _) -> bad_arg_err 2 "binop_semantics.mod" "(float, float)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.mod" "(float, float)" [ v1; v2 ])
  | Pow -> (fun v1 v2 -> match v1, v2 with
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Pow v1 v2)
            | (Real _, _) -> bad_arg_err 2 "binop_semantics.pow" "(float, float)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.pow" "(float, float)" [ v1; v2 ])
  (* FIXME: check if bitwise operators are correct (float vs int) *)
  | BitwiseAnd -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop (Ty_bitv 32) Ty.And v1 v2)
            | (Int _, _) -> bad_arg_err 2 "binop_semantics.bitwiseAnd" "(integer, integer)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.bitwiseAnd" "(integer, integer)" [ v1; v2 ])
  | BitwiseOr -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop (Ty_bitv 32) Ty.Or v1 v2)
            | (Int _, _) -> bad_arg_err 2 "binop_semantics.bitwiseOr" "(integer, integer)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.bitwiseOr" "(integer, integer)" [ v1; v2 ])
  | BitwiseXor -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop (Ty_bitv 32) Ty.Xor v1 v2)
            | (Int _, _) -> bad_arg_err 2 "binop_semantics.bitwiseXor" "(integer, integer)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.bitwiseXor" "(integer, integer)" [ v1; v2 ])
  | ShiftLeft -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop (Ty_bitv 32)  Ty.Shl v1 v2)
            | (Int _, _) -> bad_arg_err 2 "binop_semantics.shiftLeft" "(integer, integer)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.shiftLeft" "(integer, integer)" [ v1; v2 ])
  | ShiftRight -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop (Ty_bitv 32)  Ty.ShrA v1 v2)
            | (Int _, _) -> bad_arg_err 2 "binop_semantics.shiftRight" "(integer, integer)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.shiftRight" "(integer, integer)" [ v1; v2 ])
  | ShiftRightLogical -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop (Ty_bitv 32)  Ty.ShrL v1 v2)
            | (Int _, _) -> bad_arg_err 2 "binop_semantics.shiftRightExt" "(integer, integer)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.shiftRightExt" "(integer, integer)" [ v1; v2 ])
  | LogicalAnd -> (fun v1 v2 -> match v1, v2 with
            | (Value.True | Value.False), (Value.True | Value.False) -> Eval.(binop Ty_bool Ty.And v1 v2)
            | ((Value.True | Value.False), _) -> bad_arg_err 2 "binop_semantics.logicalAnd" "(boolean, boolean)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.logicalAnd" "(boolean, boolean)" [ v1; v2 ])
  | LogicalOr -> (fun v1 v2 -> match v1, v2 with
            | (Value.True | Value.False), (Value.True | Value.False) -> Eval.(binop Ty_bool Ty.Or v1 v2)
            | ((Value.True | Value.False), _) -> bad_arg_err 2 "binop_semantics.logicalOr" "(boolean, boolean)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.logicalOr" "(boolean, boolean)" [ v1; v2 ])
  | SCLogicalAnd -> failwith "binop_semantics.scLogicalAnd"
  | SCLogicalOr -> failwith "binop_semantics.scLogicalOr"
  | Eq -> ( fun v1 v2 -> make_bool (not @@ Value.equal v1 v2))
  | NE -> ( fun v1 v2 -> make_bool (not @@ Value.equal v1 v2))
  (* TODO:x how to deal? *)
  | Lt -> (fun v1 v2 -> match v1, v2 with
            | Value.Real _ , Value.Int i  -> make_bool Eval.(relop Ty_real Ty.Lt v1 (Real (float i)))
            | Value.Int i , Value.Real _  -> make_bool Eval.(relop Ty_real Ty.Lt (Real (float i)) v2)
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
  (* TODO:x index out of bounds *)
  | StringNth -> (fun v1 v2 -> match v1, v2 with
            | Value.Str _, Value.Int _ -> Eval.(binop Ty_str Ty.At v1 v2)
            | (Str _, _) -> bad_arg_err 2 "binop_semantics.stringNth" "(string, integer)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.stringNth" "(string, integer)" [ v1; v2 ])
  | ListNth | TupleNth -> (fun v1 v2 -> match v1, v2 with
            | Value.List _, Value.Int _ -> Eval.(binop Ty_list Ty.At v1 v2)
            | (List _, _) -> bad_arg_err 2 "binop_semantics.listNth/tupleNth" "(list/tuple, integer)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.listNth/tupleNth" "(list/tuple, integer)" [ v1; v2 ])
  | ListAdd -> (fun v1 v2 -> match v1, v2 with
            | Value.List _, Value.List _ -> Eval.(binop Ty_list Ty.List_append_last v1 v2)
            | _ -> bad_arg_err 1 "binop_semantics.ListAdd" "(list, any)" [ v1; v2 ])
  | ListPrepend -> (fun v1 v2 -> match v2 with
            | Value.List _ -> Eval.(binop Ty_list Ty.List_append v2 v1)
            | _ -> bad_arg_err 1 "binop_semantics.ListPrepend" "(list, any)" [ v1; v2 ])
  | ListConcat -> (fun v1 v2 -> match v1, v2 with
            | Value.List _, Value.List _ -> Eval.(naryop Ty_list Ty.Concat [v1;v2])
            | _ -> bad_arg_err 1 "binop_semantics.ListConcat" "(list, list)" [ v1; v2 ])

let triop_semantics (op: Operator.triopt) =
  match op with
  | ITE -> (fun v1 v2 v3 -> Eval.triop Ty_bool Ty.Ite v1 v2 v3)
  | StringSubstr -> (fun v1 v2 v3 -> match v1, v2, v3 with
            | Value.Str _, Value.Int _, Value.Int _ -> Eval.(triop Ty_str Ty.String_extract v1 v2 v3)
            | _ -> bad_arg_err 1 "triopt_semantics.stringSubstr" "(boolean, any, any)" [ v1; v2; v3 ])
  | ListSet -> (fun v1 v2 v3 -> match v1, v2 with
            | Value.List _, Value.Int _-> Eval.(triop Ty_list Ty.List_set v1 v2 v3)
            | List _, _ -> bad_arg_err 2 "triopt_semantics.listSet" "(list, integer, any)" [ v1; v2; v3 ]
            | _ -> bad_arg_err 1 "triopt_semantics.listSet" "(list, integer, any)" [ v1; v2; v3 ])

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