module Op = Operators

exception Unknown
exception Error of string

let time_solver = ref 0.0

let ctx =
  Z3.mk_context
    [ ("model", "true"); ("proof", "false"); ("unsat_core", "false") ]

let fp_sort = Z3.FloatingPoint.mk_sort_32 ctx
let int_sort = Z3.Arithmetic.Integer.mk_sort ctx

(*let real_sort = Z3.Arithmetic.Real.mk_sort ctx*)
let bool_sort = Z3.Boolean.mk_sort ctx

(* Rouding modes *)
let rne = Z3.FloatingPoint.RoundingMode.mk_rne ctx
(*let rtz = Z3.FloatingPoint.RoundingMode.mk_rtz ctx*)

let sort_of_type (ctx : Z3.context) (t : Type.t) : Z3.Sort.sort =
  match t with
  | Type.IntType -> int_sort
  | Type.FltType -> fp_sort
  | Type.BoolType -> bool_sort
  | _ -> failwith "Encoding: sort_of_type: Unsupported type!"

let arith_unop (op : Op.uopt) : Z3.Expr.expr -> Z3.Expr.expr =
  match op with
  | Op.Not -> Z3.Boolean.mk_not ctx
  | Op.Neg -> Z3.Arithmetic.mk_unary_minus ctx
  | _ -> failwith "Encoding: arith_unop: not implemented!"

let fp_unop (op : Op.uopt) : Z3.Expr.expr -> Z3.Expr.expr =
  match op with
  | Op.Not -> Z3.Boolean.mk_not ctx
  | Op.Neg -> Z3.FloatingPoint.mk_neg ctx
  | _ -> failwith "Encoding: fp_unop: not implemente!"

let encode_unop (op : Op.uopt) (v : Z3.Expr.expr) : Z3.Expr.expr =
  let op' = if Z3.FloatingPoint.is_fp v then fp_unop op else arith_unop op in
  op' v

let arith_binop (op : Op.bopt) : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr =
  match op with
  | Op.Eq -> Z3.Boolean.mk_eq ctx
  | Op.Gt -> Z3.Arithmetic.mk_gt ctx
  | Op.Lt -> Z3.Arithmetic.mk_lt ctx
  | Op.Ge -> Z3.Arithmetic.mk_ge ctx
  | Op.Le -> Z3.Arithmetic.mk_le ctx
  | Op.Log_And -> fun v1 v2 -> Z3.Boolean.mk_and ctx [ v1; v2 ]
  | Op.Plus -> fun v1 v2 -> Z3.Arithmetic.mk_add ctx [ v1; v2 ]
  | Op.Times -> fun v1 v2 -> Z3.Arithmetic.mk_mul ctx [ v1; v2 ]
  | Op.Div -> Z3.Arithmetic.mk_div ctx
  | _ ->
      failwith
        ("Encoding: encode_binop: '" ^ Op.str_of_binopt_single op
       ^ "' not implemented!")

let fp_binop (op : Op.bopt) : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr =
  match op with
  | Op.Eq -> Z3.Boolean.mk_eq ctx
  | Op.Gt -> Z3.FloatingPoint.mk_gt ctx
  | Op.Lt -> Z3.FloatingPoint.mk_lt ctx
  | Op.Ge -> Z3.FloatingPoint.mk_geq ctx
  | Op.Le -> Z3.FloatingPoint.mk_leq ctx
  | Op.Log_And -> fun v1 v2 -> Z3.Boolean.mk_and ctx [ v1; v2 ]
  | Op.Plus -> Z3.FloatingPoint.mk_add ctx rne
  | Op.Times -> Z3.FloatingPoint.mk_mul ctx rne
  | Op.Div -> Z3.FloatingPoint.mk_div ctx rne
  | _ ->
      failwith
        ("Encoding: encode_binop: '" ^ Op.str_of_binopt_single op
       ^ "' not implemented!")

let encode_binop (op : Op.bopt) (v1 : Z3.Expr.expr) (v2 : Z3.Expr.expr) :
    Z3.Expr.expr =
  let op' =
    if Z3.FloatingPoint.is_fp v1 || Z3.FloatingPoint.is_fp v2 then fp_binop op
    else arith_binop op
  in
  op' v1 v2

let rec encode_value (v : Sval.t) : Z3.Expr.expr =
  match v with
  | Sval.Int i -> Z3.Arithmetic.Integer.mk_numeral_i ctx i
  | Sval.Flt f -> Z3.FloatingPoint.mk_numeral_f ctx f fp_sort
  | Sval.Bool b -> Z3.Boolean.mk_val ctx b
  | Sval.Byte i -> Z3.Arithmetic.Integer.mk_numeral_i ctx i
  | Sval.Symbolic (t, x) -> Z3.Expr.mk_const_s ctx x (sort_of_type ctx t)
  | Sval.Unop (op, v) ->
      let v' = encode_value v in
      encode_unop op v'
  | Sval.Binop (op, v1, v2) ->
      let v1' = encode_value v1 and v2' = encode_value v2 in
      encode_binop op v1' v2'
  | _ ->
      failwith ("Encoding: encode_value: '" ^ Sval.str v ^ "' not implemented!")

let mk_solver () : Z3.Solver.solver = Z3.Solver.mk_solver ctx None

let clone (solver : Z3.Solver.solver) : Z3.Solver.solver =
  Z3.Solver.translate solver ctx

let add (solver : Z3.Solver.solver) (vs : Sval.t list) : unit =
  try
    List.iter (fun v -> Logging.print_endline (lazy ("Add: " ^ Sval.str v))) vs;
    let vs' = List.map encode_value vs in
    Z3.Solver.add solver vs'
  with Z3.Error e -> raise (Error e)

let pop (solver : Z3.Solver.solver) (lvl : int) : unit =
  Z3.Solver.pop solver lvl

let push (solver : Z3.Solver.solver) : unit = Z3.Solver.push solver

let check (solver : Z3.Solver.solver) (vs : Sval.t list) : bool =
  try
    let vs' = List.map encode_value vs in
    List.iter
      (fun e -> Logging.print_endline (lazy (Z3.Expr.to_string e)))
      (vs' @ Z3.Solver.get_assertions solver);
    let b =
      let sat =
        Time_utils.time_call time_solver (fun () -> Z3.Solver.check solver vs')
      in
      match sat with
      | Z3.Solver.SATISFIABLE -> true
      | Z3.Solver.UNKNOWN -> raise Unknown
      | Z3.Solver.UNSATISFIABLE -> false
    in
    Logging.print_endline
      (lazy ("leaving check with return " ^ string_of_bool b));
    b
  with Z3.Error e -> raise (Error e)

let model (solver : Z3.Solver.solver) (vs : Sval.t list) :
    (Z3.Sort.sort * Z3.Symbol.symbol * Z3.Expr.expr option) list =
  assert (check solver vs);
  match Z3.Solver.get_model solver with
  | None -> assert false
  | Some model ->
      Logging.print_endline (lazy (Z3.Model.to_string model));
      List.map
        (fun const ->
          let sort = Z3.FuncDecl.get_range const
          and name = Z3.FuncDecl.get_name const
          and interp = Z3.Model.get_const_interp model const in
          (sort, name, interp))
        (Z3.Model.get_const_decls model)

let string_of_value (e : Z3.Expr.expr) : string =
  let f =
    match Z3.Sort.get_sort_kind (Z3.Expr.get_sort e) with
    | Z3enums.INT_SORT -> Z3.Arithmetic.Integer.numeral_to_string
    | Z3enums.FLOATING_POINT_SORT -> Z3.FloatingPoint.numeral_to_string
    | _ -> Z3.Expr.to_string
  in
  f e
