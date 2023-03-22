open Encode_unop
open Encode_binop
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

let str_sort = Z3.Seq.mk_string_sort ctx

(* Rouding modes *)
let rne = Z3.FloatingPoint.RoundingMode.mk_rne ctx
(*let rtz = Z3.FloatingPoint.RoundingMode.mk_rtz ctx*)

let sort_of_type (ctx : Z3.context) (t : Type.t) : Z3.Sort.sort =
  match t with
  | Type.IntType -> int_sort
  | Type.FltType -> fp_sort
  | Type.BoolType -> bool_sort
  | Type.StrType -> str_sort
  | _ -> failwith "Encoding: sort_of_type: Unsupported type!"

let encode_unop (op : Op.uopt) (v : Sval.t) (encoded_v : Z3.Expr.expr) : Z3.Expr.expr =
  let op' = 
  match op with 
  | Op.StringLen -> Z3.Seq.mk_seq_length ctx
  | Op.Neg -> encode_neg v ctx
  | Op.Not -> Z3.Boolean.mk_not ctx
  | _ -> failwith "Encoding: unop not implemented!"
  in op' encoded_v

let encode_binop (op : Op.bopt) (v1 : Sval.t) (v2 : Sval.t) (v1_encoded : Z3.Expr.expr) (v2_encoded : Z3.Expr.expr) : Z3.Expr.expr =
  let v_type = Sval_typing.type_of v1 in
  (*FIXME: check if both values have the same type*)
  let op' =
  match op with
  | Op.Eq -> Z3.Boolean.mk_eq ctx
  | Op.Gt -> encode_gt v_type ctx 
  | Op.Lt -> encode_lt v_type ctx
  | Op.Ge -> encode_ge v_type ctx
  | Op.Le -> encode_le v_type ctx
  | Op.Log_And -> fun v1 v2 -> Z3.Boolean.mk_and ctx [ v1; v2 ]
  | Op.Plus -> encode_add v_type ctx rne
  | Op.Times -> encode_times v_type ctx rne
  | Op.Div -> encode_div v_type ctx rne
  | Op.Snth -> fun v1 v2 -> Z3.Seq.mk_seq_extract ctx v1 v2 (Z3.Arithmetic.Integer.mk_numeral_i ctx 1)
  | _ -> failwith "Encoding: binop not implemented."
  in
  op' v1_encoded v2_encoded

let rec encode_value (v : Sval.t) : Z3.Expr.expr =
  match v with
  | Sval.Int i -> Z3.Arithmetic.Integer.mk_numeral_i ctx i
  | Sval.Flt f -> Z3.FloatingPoint.mk_numeral_f ctx f fp_sort
  | Sval.Bool b -> Z3.Boolean.mk_val ctx b
  | Sval.Byte i -> Z3.Arithmetic.Integer.mk_numeral_i ctx i
  | Sval.Str s -> Z3.Seq.mk_string ctx s
  | Sval.Symbolic (t, x) -> Z3.Expr.mk_const_s ctx x (sort_of_type ctx t)
  | Sval.List l ->
      raise (Error ("encode_value: List '" ^ Sval.str v ^ "' not implemented"))
  | Sval.Unop (op, v) ->
      let v' = encode_value v in
      encode_unop op v v' 
  | Sval.Binop (op, v1, v2) ->
      let v1' = encode_value v1 and v2' = encode_value v2 in
      encode_binop op v1 v2 v1' v2'
  | _ ->
      failwith ("Encoding: encode_value: '" ^ Sval.str v ^ "' not implemented!")

let mk_solver () : Z3.Solver.solver = Z3.Solver.mk_solver ctx None

let mk_opt () : Z3.Optimize.optimize = Z3.Optimize.mk_opt ctx

let clone (solver : Z3.Solver.solver) : Z3.Solver.solver =
  Z3.Solver.translate solver ctx

let add (solver : Z3.Solver.solver) (vs : Sval.t list) : unit =
  try
    List.iter (fun v -> Logging.print_endline (lazy ("Add: " ^ Sval.str v))) vs;
    let vs' = List.map encode_value vs in
    Z3.Solver.add solver vs'
  with Z3.Error e -> raise (Error e)

let add_opt (opt : Z3.Optimize.optimize) (vs : Sval.t list) : unit =
  List.iter (fun v -> Logging.print_endline (lazy ("Add: " ^ Sval.str v))) vs;
  let vs' = List.map encode_value vs in
  Z3.Optimize.add opt vs'

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

let castValue (expr : Z3.Expr.expr) (expr_type : Type.t) : Sval.t =
  match expr_type with
  | Type.IntType -> Sval.Int (Big_int_Z.int_of_big_int (Z3.Arithmetic.Integer.get_big_int expr))
  | Type.FltType -> Sval.Flt(Q.to_float (Z3.Arithmetic.Real.get_ratio expr))
  | _ -> Sval.Str (Z3.Expr.to_string expr)

let optimize (optimize : Z3.Optimize.optimize) (expr : Sval.t) (expr_type : Type.t)
  (vs : Sval.t list) (f : Z3.Optimize.optimize -> Z3.Expr.expr -> Z3.Optimize.handle) : Sval.t =
let _ = Z3.Optimize.push optimize in
let _ = add_opt optimize vs in
let h = f optimize (encode_value expr) in
let ret =
  let sat =
    Time_utils.time_call time_solver (fun () -> Z3.Optimize.check optimize)
  in
  match sat with
  | Z3.Solver.SATISFIABLE -> castValue (Z3.Optimize.get_upper h) expr_type
  | _ -> Sval.Null
in let _ = Z3.Optimize.pop optimize in ret


let maximize (opt : Z3.Optimize.optimize) (expr : Sval.t) (expr_type : Type.t) 
    (vs : Sval.t list) =
  optimize opt expr expr_type vs Z3.Optimize.maximize

let minimize (opt : Z3.Optimize.optimize) (expr : Sval.t) (expr_type : Type.t)
    (vs : Sval.t list) =
  optimize opt expr expr_type vs Z3.Optimize.minimize

let get_const_interp (solver : Z3.Solver.solver) (v : Sval.t) (expr_type : Type.t) (vs : Sval.t list) =
  assert (check solver vs);
  let model = Option.get (Z3.Solver.get_model solver) in
  let res = Option.get (Z3.Model.eval model (encode_value v) true) in
  castValue res expr_type 
