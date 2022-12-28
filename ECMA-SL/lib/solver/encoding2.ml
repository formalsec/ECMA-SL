open Val
open Expr
open Oper
module Arithmetic = Z3.Arithmetic
module Boolean = Z3.Boolean
module Datatype = Z3.Datatype
module Enumeration = Z3.Enumeration
module FloatingPoint = Z3.FloatingPoint
module FuncDecl = Z3.FuncDecl
module Model = Z3.Model
module Solver = Z3.Solver
module Symbol = Z3.Symbol
module ZExpr = Z3.Expr

(*  Encoding option  *)
type encoding = WithReals | WithFPA

let encoding = ref WithReals
let match_enc msg x y = match !encoding with WithReals -> x | WithFPA -> y

(*  Configurations  *)
let cfg =
  [
    ("model", "true");
    ("proof", "true");
    ("unsat_core", "true");
    ("timeout", "16384");
  ]

let ctx : Z3.context = Z3.mk_context cfg

(*  Sorting  *)
let booleans_sort = Boolean.mk_sort ctx
let ints_sort = Arithmetic.Integer.mk_sort ctx
let reals_sort = Arithmetic.Real.mk_sort ctx
let fp_sort = FloatingPoint.mk_sort_64 ctx
let numbers_sort = match_enc "mk_sort" reals_sort fp_sort

let rm =
  Z3.FloatingPoint.mk_const ctx
    (Z3.Symbol.mk_string ctx "rm")
    (Z3.FloatingPoint.RoundingMode.mk_sort ctx)

let mk_string_symb s = Z3.Symbol.mk_string ctx s

(*  ESL Values : constructors  *)
type esl_type_constructors = {
  boolean_type_constructor : FuncDecl.func_decl;
  int_type_constructor : FuncDecl.func_decl;
  number_type_constructor : FuncDecl.func_decl;
  string_type_constructor : FuncDecl.func_decl;
}

(*  ESL Values CONSTRUCTORS, ACCESSORS AND RECOGNIZERS  *)
type z3_values = {
  (*  Constructors  *)
  bool_constructor : Z3.FuncDecl.func_decl;
  num_constructor : Z3.FuncDecl.func_decl;
  string_constructor : Z3.FuncDecl.func_decl;
  int_constructor : Z3.FuncDecl.func_decl;
  (*  Accessors  *)
  bool_accessor : Z3.FuncDecl.func_decl;
  num_accessor : Z3.FuncDecl.func_decl;
  string_accessor : Z3.FuncDecl.func_decl;
  int_accessor : Z3.FuncDecl.func_decl;
  (*  Recognizers  *)
  bool_recognizer : Z3.FuncDecl.func_decl;
  num_recognizer : Z3.FuncDecl.func_decl;
  string_recognizer : Z3.FuncDecl.func_decl;
  int_recognizer : Z3.FuncDecl.func_decl;
}

(*  ---- OPERATIONS ----  *)

(*  Real/FPA operations  *)
let mk_const =
  match_enc "mk_const" (Z3.Arithmetic.Real.mk_const ctx)
    (fun (s : Z3.Symbol.symbol) -> Z3.FloatingPoint.mk_const ctx s fp_sort)

let mk_num_i =
  match_enc "mk_num_i" (Z3.Arithmetic.Real.mk_numeral_i ctx) (fun i ->
      Z3.FloatingPoint.mk_numeral_i ctx i fp_sort)

let mk_num_s =
  match_enc "mk_num_s" (Z3.Arithmetic.Real.mk_numeral_s ctx) (fun s ->
      Z3.FloatingPoint.mk_numeral_s ctx s fp_sort)

let mk_lt = match_enc "mk_lt" Z3.Arithmetic.mk_lt Z3.FloatingPoint.mk_lt
let mk_le = match_enc "mk_le" Z3.Arithmetic.mk_le Z3.FloatingPoint.mk_leq
let mk_ge = match_enc "mk_ge" Z3.Arithmetic.mk_ge Z3.FloatingPoint.mk_geq
let mk_gt = match_enc "mk_gt" Z3.Arithmetic.mk_gt Z3.FloatingPoint.mk_gt

let mk_add =
  match_enc "mk_add"
    (fun e1 e2 -> Z3.Arithmetic.mk_add ctx [ e1; e2 ])
    (fun e1 e2 -> Z3.FloatingPoint.mk_add ctx rm e1 e2)

let mk_sub =
  match_enc "mk_sub"
    (fun e1 e2 -> Z3.Arithmetic.mk_sub ctx [ e1; e2 ])
    (fun e1 e2 -> Z3.FloatingPoint.mk_sub ctx rm e1 e2)

let mk_mul =
  match_enc "mk_mul"
    (fun e1 e2 -> Z3.Arithmetic.mk_mul ctx [ e1; e2 ])
    (fun e1 e2 -> Z3.FloatingPoint.mk_mul ctx rm e1 e2)

let mk_div =
  match_enc "mk_div"
    (fun e1 e2 -> Z3.Arithmetic.mk_div ctx e1 e2)
    (fun e1 e2 -> Z3.FloatingPoint.mk_div ctx rm e1 e2)

(*  Integer operations  *)
let mk_int_i = Z3.Arithmetic.Integer.mk_numeral_i ctx
let mk_int_s = Z3.Arithmetic.Integer.mk_numeral_s ctx
let mk_add_i e1 e2 = Z3.Arithmetic.mk_add ctx [ e1; e2 ]
let mk_sub_i e1 e2 = Z3.Arithmetic.mk_sub ctx [ e1; e2 ]
let mk_mul_i e1 e2 = Z3.Arithmetic.mk_mul ctx [ e1; e2 ]
let mk_div_i e1 e2 = Z3.Arithmetic.mk_div ctx e1 e2
let mk_lt_i = Z3.Arithmetic.mk_lt
let mk_le_i = Z3.Arithmetic.mk_le
let mk_ge_i = Z3.Arithmetic.mk_ge
let mk_gt_i = Z3.Arithmetic.mk_gt

(*  ---- ESL Z3 Sorts ----  *)

let z3_esl_literal_sort, esl_lit_operations =
  (*  esl type constructors  *)
  let esl_bool_constructor =
    Z3.Datatype.mk_constructor ctx (mk_string_symb "Bool")
      (mk_string_symb "isBool")
      [ mk_string_symb "bValue" ]
      [ Some booleans_sort ] [ 0 ]
  in
  let esl_num_constructor =
    Z3.Datatype.mk_constructor ctx (mk_string_symb "Num")
      (mk_string_symb "isNum")
      [ mk_string_symb "nValue" ]
      [ Some numbers_sort ] [ 0 ]
  in
  let esl_string_constructor =
    Z3.Datatype.mk_constructor ctx (mk_string_symb "String")
      (mk_string_symb "isString")
      [ mk_string_symb "sValue" ]
      [ Some ints_sort ] [ 0 ]
  in
  let esl_int_constructor =
    Z3.Datatype.mk_constructor ctx (mk_string_symb "Int")
      (mk_string_symb "isInt")
      [ mk_string_symb "iValue" ]
      [ Some ints_sort ] [ 0 ]
  in

  let esl_sort =
    Z3.Datatype.mk_sort ctx
      (mk_string_symb "ESL_Literal")
      [
        esl_bool_constructor;
        esl_num_constructor;
        esl_string_constructor;
        esl_int_constructor;
      ]
  in

  try
    (*  Constructors  *)
    let z3_esl_constructors = Z3.Datatype.get_constructors esl_sort in
    let bool_constructor = List.nth z3_esl_constructors 0 in
    let num_constructor = List.nth z3_esl_constructors 1 in
    let string_constructor = List.nth z3_esl_constructors 2 in
    let int_constructor = List.nth z3_esl_constructors 3 in

    (*  Accessors  *)
    let z3_esl_accessors = Z3.Datatype.get_accessors esl_sort in
    let bool_accessor = List.nth (List.nth z3_esl_accessors 0) 0 in
    let num_accessor = List.nth (List.nth z3_esl_accessors 1) 0 in
    let string_accessor = List.nth (List.nth z3_esl_accessors 2) 0 in
    let int_accessor = List.nth (List.nth z3_esl_accessors 3) 0 in

    (*  Recognizers  *)
    let z3_esl_recognizers = Z3.Datatype.get_recognizers esl_sort in
    let bool_recognizer = List.nth z3_esl_recognizers 0 in
    let num_recognizer = List.nth z3_esl_recognizers 1 in
    let string_recognizer = List.nth z3_esl_recognizers 2 in
    let int_recognizer = List.nth z3_esl_recognizers 3 in

    let esl_literal_operations =
      {
        (*  Constructors  *)
        bool_constructor;
        num_constructor;
        string_constructor;
        int_constructor;
        (*  Accessors  *)
        bool_accessor;
        num_accessor;
        string_accessor;
        int_accessor;
        (*  Recognizers  *)
        bool_recognizer;
        num_recognizer;
        string_recognizer;
        int_recognizer;
      }
    in
    (esl_sort, esl_literal_operations)
  with _ -> raise (Failure "DEATH: construction of z3_esl_sort")

(*  String codes  *)
let str_codes = Hashtbl.create 1000
let str_codes_inv = Hashtbl.create 1000
let str_counter = ref 0

(*  Encode strings into integer values -> z3_code  *)
let encode_string (str : string) : Z3.Expr.expr =
  try
    let str_number = Hashtbl.find str_codes str in
    let z3_code = mk_int_i str_number in
    z3_code
  with Not_found ->
    (* New string: add it to the hashtable *)
    let z3_code = mk_int_i !str_counter in
    Hashtbl.add str_codes str !str_counter;
    Hashtbl.add str_codes_inv !str_counter str;
    str_counter := !str_counter + 1;
    z3_code

(*  ---- Value Encoding ----  *)

let encode_val (v : Val.t) : ZExpr.expr =
  try
    match v with
    | Bool b ->
        let b_arg =
          match b with
          | true -> Boolean.mk_true ctx
          | false -> Boolean.mk_false ctx
        in
        ZExpr.mk_app ctx esl_lit_operations.bool_constructor [ b_arg ]
    | Flt n ->
        let sfn = string_of_float n in
        let n_arg = mk_num_s sfn in
        ZExpr.mk_app ctx esl_lit_operations.num_constructor [ n_arg ]
    | Int i ->
        let sfn = string_of_int i in
        let n_arg = mk_int_s sfn in
        Z3.Expr.mk_app ctx esl_lit_operations.int_constructor [ n_arg ]
    | Str s ->
        let s_arg = encode_string s in
        ZExpr.mk_app ctx esl_lit_operations.string_constructor [ s_arg ]
    | _ -> raise (Failure "Unsupported Value")
  with Failure msg ->
    raise (Failure (Printf.sprintf "DEATH: encode_lit %s. %s" (Val.str v) msg))

(*  ---- Expression Encoding ----  *)

let encode_unop (op : Oper.uopt) (e : ZExpr.expr) : ZExpr.expr =
  match op with
  | Not ->
      let e' = ZExpr.mk_app ctx esl_lit_operations.bool_accessor [ e ] in
      let e'' = Z3.Boolean.mk_not ctx e' in
      Z3.Expr.mk_app ctx esl_lit_operations.bool_constructor [ e'' ]
  | _ -> raise (Failure (Printf.sprintf "unop fail"))

let binop_numbers_to_numbers (mk_op : ZExpr.expr -> ZExpr.expr -> ZExpr.expr)
    (e1 : ZExpr.expr) (e2 : ZExpr.expr) : ZExpr.expr =
  let e1' = ZExpr.mk_app ctx esl_lit_operations.num_accessor [ e1 ] in
  let e2' = ZExpr.mk_app ctx esl_lit_operations.num_accessor [ e2 ] in
  let e' = mk_op e1' e2' in
  ZExpr.mk_app ctx esl_lit_operations.num_constructor [ e' ]

let binop_numbers_to_booleans (mk_op : ZExpr.expr -> ZExpr.expr -> ZExpr.expr)
    (e1 : ZExpr.expr) (e2 : ZExpr.expr) : ZExpr.expr =
  let e1' = ZExpr.mk_app ctx esl_lit_operations.num_accessor [ e1 ] in
  let e2' = ZExpr.mk_app ctx esl_lit_operations.num_accessor [ e2 ] in
  let e' = mk_op e1' e2' in
  ZExpr.mk_app ctx esl_lit_operations.bool_constructor [ e' ]

let binop_booleans_to_booleans (mk_op : ZExpr.expr list -> ZExpr.expr)
    (e1 : ZExpr.expr) (e2 : ZExpr.expr) : ZExpr.expr =
  let e1' = ZExpr.mk_app ctx esl_lit_operations.bool_accessor [ e1 ] in
  let e2' = ZExpr.mk_app ctx esl_lit_operations.bool_accessor [ e2 ] in
  let e' = mk_op [ e1'; e2' ] in
  ZExpr.mk_app ctx esl_lit_operations.bool_constructor [ e' ]

let encode_binop (op : Oper.bopt) (e1 : ZExpr.expr) (e2 : ZExpr.expr) :
    ZExpr.expr =
  match op with
  | Plus -> binop_numbers_to_numbers mk_add e1 e2
  | Minus -> binop_numbers_to_numbers mk_sub e1 e2
  | Times -> binop_numbers_to_numbers mk_mul e1 e2
  | Div -> binop_numbers_to_numbers mk_div e1 e2
  | Equal ->
      ZExpr.mk_app ctx esl_lit_operations.bool_constructor
        [ Boolean.mk_eq ctx e1 e2 ]
  | Gt -> binop_numbers_to_booleans (mk_gt ctx) e1 e2
  | Lt -> binop_numbers_to_booleans (mk_lt ctx) e1 e2
  | Egt -> binop_numbers_to_booleans (mk_ge ctx) e1 e2
  | Elt -> binop_numbers_to_booleans (mk_le ctx) e1 e2
  | Log_And -> binop_booleans_to_booleans (Boolean.mk_and ctx) e1 e2
  | Log_Or -> binop_booleans_to_booleans (Boolean.mk_or ctx) e1 e2
  | _ -> raise (Failure "encode_binop")

let encode_triop (op : Oper.topt) (e1 : ZExpr.expr) (e2 : ZExpr.expr)
    (e3 : ZExpr.expr) : ZExpr.expr =
  raise (Failure "encode_triop")

let encode_nop (op : Oper.nopt) (es : ZExpr.expr list) : ZExpr.expr =
  raise (Failure "encode_nop")

let rec encode_expr (e : Expr.t) : ZExpr.expr =
  let fe = encode_expr in

  match e with
  | Val v -> encode_val v
  | Var x -> ZExpr.mk_const ctx (mk_string_symb x) z3_esl_literal_sort
  | UnOpt (op, e) -> encode_unop op (fe e)
  | BinOpt (op, e1, e2) -> encode_binop op (fe e1) (fe e2)
  | TriOpt (op, e1, e2, e3) -> encode_triop op (fe e1) (fe e2) (fe e3)
  | NOpt (op, es) -> encode_nop op (List.map fe es)
  | _ ->
      raise
        (Failure
           (Printf.sprintf "Unsupported Expression. encode_expr: %s\n"
              (Expr.str e)))

let check_unsat (es : Expr.t list) : bool =
  Printf.printf "starting unsat check with exprs: %s\n"
    (String.concat ", " (List.map Expr.str es));

  try
    let es' = List.map encode_expr es in
    let es'' =
      List.map
        (fun e -> ZExpr.mk_app ctx esl_lit_operations.bool_accessor [ e ])
        es'
    in

    let masterSolver = Z3.Solver.mk_solver ctx None in

    Z3.Solver.add masterSolver es'';

    let ret = Z3.Solver.check masterSolver [] in

    let b = ret = Z3.Solver.UNSATISFIABLE in

    Printf.printf "leaving unsat check with return %b\n" b;

    b
  with Failure msg ->
    Printf.printf "leaving unsat check with exception %s\n" msg;
    false
