open Sval

type t = Sval.t

module Op = Operators

let eval_unop (op : Op.uopt) (v : t) : t =
  match op with
  | Op.Neg -> (
      match v with
      | Flt f -> Flt (-.f)
      | Int i -> Int (-i)
      | _ -> Unop (Op.Neg, v))
  | Op.Not -> ( match v with Bool b -> Bool (not b) | _ -> Unop (Op.Not, v))
  | _ -> failwith "Eval_operations: eval_unop not implemented"

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
  | Op.Log_And -> (
      match (v1, v2) with
      | Bool b1, Bool b2 -> Bool (b1 && b2)
      | _ -> Binop (op, v1, v2))
  | Op.Log_Or -> (
      match (v1, v2) with
      | Bool b1, Bool b2 -> Bool (b1 || b2)
      | _ -> Binop (op, v1, v2))
  | _ ->
      failwith
        ("Eval_operations: eval_binop: " ^ Op.str_of_binopt_single op
       ^ " not implemented!")

let eval_triop (op : Op.topt) (v1 : t) (v2 : t) (v1 : t) : t =
  failwith "Eval_operations: eval_triop not implemented"

let eval_nop (op : Op.nopt) (vs : t list) : t =
  match op with
  | Op.TupleExpr -> Tuple vs
  | _ ->
      failwith
        ("Eval_operations: eval_nop: " ^ Op.str_of_nopt op [ "" ]
       ^ " not implemented")
