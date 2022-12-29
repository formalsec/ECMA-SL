open Operators

let eval_unop (op : uopt) (v : Sval.t) : Sval.t =
  match op with
  | Not -> Sval.not v
  | _ -> failwith "Eval_operations: eval_unop not implemented"

let eval_binop (op : bopt) (v1 : Sval.t) (v2 : Sval.t) : Sval.t =
  match op with
  | Lt -> Sval.lt v1 v2
  | Gt -> Sval.gt v1 v2
  | Times -> Sval.times v1 v2
  | _ ->
      failwith
        ("Eval_operations: eval_binop: "
        ^ str_of_binopt_single op
        ^ " not implemented!")

let eval_triop (op : topt) (v1 : Sval.t) (v2 : Sval.t) (v1 : Sval.t) :
    Sval.t =
  failwith "Eval_operations: eval_triop not implemented"

let eval_nop (op : nopt) (vs : Sval.t list) : Sval.t =
  match op with
  | TupleExpr -> Sval.Tuple vs
  | _ ->
      failwith
        ("Eval_operations: eval_nop: " ^ str_of_nopt op [ "" ]
       ^ " not implemented")
