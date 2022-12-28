let eval_unop (op : Oper.uopt) (v : Sval.t) : Sval.t =
  match op with
  | Oper.Not -> Sval.not v
  | _ -> failwith "Eval_operations: eval_unop not implemented"

let eval_binop (op : Oper.bopt) (v1 : Sval.t) (v2 : Sval.t) : Sval.t =
  match op with
  | Oper.Lt -> Sval.lt v1 v2
  | Oper.Gt -> Sval.gt v1 v2
  | Oper.Times -> Sval.times v1 v2
  | _ ->
      failwith
        ("Eval_operations: eval_binop: "
        ^ Oper.str_of_binopt_single op
        ^ " not implemented!")

let eval_triop (op : Oper.topt) (v1 : Sval.t) (v2 : Sval.t) (v1 : Sval.t) :
    Sval.t =
  failwith "Eval_operations: eval_triop not implemented"

let eval_nop (op : Oper.nopt) (vs : Sval.t list) : Sval.t =
  match op with
  | Oper.TupleExpr -> Sval.Tuple vs
  | _ ->
      failwith
        ("Eval_operations: eval_nop: " ^ Oper.str_of_nopt op [ "" ]
       ^ " not implemented")
