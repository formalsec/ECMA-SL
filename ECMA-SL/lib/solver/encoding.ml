open Operators

exception Unknown

let cfg = [ ("model", "false"); ("proof", "false"); ("unsat_core", "false") ]
let ctx : Z3.context = Z3.mk_context cfg
let int_sort = Z3.Arithmetic.Integer.mk_sort ctx

let encode_unop (op : uopt) (v : Z3.Expr.expr) : Z3.Expr.expr =
  let f =
    match op with
    | Not -> Z3.Boolean.mk_not ctx
    | _ -> failwith "Encoding: encode_unop: not implemented!"
  in
  f v

let encode_binop (op : bopt) (v1 : Z3.Expr.expr) (v2 : Z3.Expr.expr) :
    Z3.Expr.expr =
  let f =
    match op with
    | Eq -> Z3.Boolean.mk_eq ctx
    | Gt -> Z3.Arithmetic.mk_gt ctx
    | Lt -> Z3.Arithmetic.mk_lt ctx
    | Ge -> Z3.Arithmetic.mk_ge ctx
    | Le -> Z3.Arithmetic.mk_le ctx
    | Log_And -> fun v1 v2 -> Z3.Boolean.mk_and ctx [ v1; v2 ]
    | Plus -> fun v1 v2 -> Z3.Arithmetic.mk_add ctx [ v1; v2 ]
    | Times -> fun v1 v2 -> Z3.Arithmetic.mk_mul ctx [ v1; v2 ]
    | _ ->
        failwith ("Encoding: encode_binop: '"
          ^ (str_of_binopt_single  op) ^ "' not implemented!")
  in
  f v1 v2

let rec encode_value (v : Sval.t) : Z3.Expr.expr =
  match v with
  | Sval.Int i -> Z3.Arithmetic.Integer.mk_numeral_i ctx i
  | Sval.Symbolic (t, x) -> Z3.Expr.mk_const_s ctx x int_sort
  | Sval.Unop (op, v) ->
      let v' = encode_value v in
      encode_unop op v'
  | Sval.Binop (op, v1, v2) ->
      let v1' = encode_value v1 and v2' = encode_value v2 in
      encode_binop op v1' v2'
  | _ ->
      failwith ("Encoding: encode_value: '" ^ Sval.str v ^ "' not implemented!")

let check (vs : Sval.t list) : bool =
  let vs' = List.map encode_value vs in
  List.iter (fun e -> Logging.print_endline (lazy (Z3.Expr.to_string e))) vs';
  let solver = Z3.Solver.mk_solver ctx None in
  let _ = Z3.Solver.add solver vs' in
  let b =
    match Z3.Solver.check solver [] with
    | Z3.Solver.SATISFIABLE -> true
    | Z3.Solver.UNKNOWN -> raise Unknown
    | Z3.Solver.UNSATISFIABLE -> false
  in
  Logging.print_endline (lazy ("leaving check with return " ^ string_of_bool b));
  b
