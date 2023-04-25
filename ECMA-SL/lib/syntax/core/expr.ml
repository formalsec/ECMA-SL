open Core

type t =
  | Val of Val.t
  | Var of string
  | Symbolic of Type.t * t
  | UnOpt of (Operators.uopt * t)
  | BinOpt of (Operators.bopt * t * t)
  | TriOpt of (Operators.topt * t * t * t)
  | NOpt of Operators.nopt * t list
  | Curry of t * t list

let rec equal (e1 : t) (e2 : t) : bool =
  match (e1, e2) with
  | Val v1, Val v2 -> Val.equal v1 v2
  | Var x1, Var x2 -> String.equal x1 x2
  | Symbolic (t1, e1'), Symbolic (t2, e2') -> Type.(t1 = t2) && equal e1' e2'
  | UnOpt (op1, e1'), UnOpt (op2, e2') -> Caml.( = ) op1 op2 && equal e1' e2'
  | BinOpt (op1, e1', e2'), BinOpt (op2, e3', e4') ->
      Caml.( = ) op1 op2 && equal e1' e3' && equal e2' e4'
  | TriOpt (op1, e1', e2', e3'), TriOpt (op2, e4', e5', e6') ->
      Caml.( = ) op1 op2 && equal e1' e4' && equal e2' e5' && equal e3' e6'
  | NOpt (op1, es1), NOpt (op2, es2) ->
      Caml.( = ) op1 op2 && List.equal equal es1 es2
  | Curry (x1, es1), Curry (x2, es2) -> equal x1 x2 && List.equal equal es1 es2
  | _ -> false

let is_loc (v : t) : bool = match v with Val (Val.Loc _) -> true | _ -> false

let rec is_symbolic (v : t) : bool =
  match v with
  | Val _ | Var _ -> false
  | Symbolic (t, x) -> true
  | UnOpt (_, v) -> is_symbolic v
  | BinOpt (_, v1, v2) -> is_symbolic v1 || is_symbolic v2
  | TriOpt (_, v1, v2, v3) -> List.for_all ~f:is_symbolic [ v1; v2; v3 ]
  | NOpt (_, es) | Curry (_, es) ->
      (not (List.is_empty es)) && List.for_all ~f:is_symbolic es

let rec str (e : t) : string =
  let str_es es = String.concat ~sep:", " (List.map ~f:str es) in
  match e with
  | Val n -> Val.str n
  | Var x -> x
  | UnOpt (op, e) -> Operators.str_of_unopt op ^ "(" ^ str e ^ ")"
  | BinOpt (op, e1, e2) -> Operators.str_of_binopt op (str e1) (str e2)
  | TriOpt (op, e1, e2, e3) ->
      Operators.str_of_triopt op (str e1) (str e2) (str e3)
  | NOpt (op, es) -> Operators.str_of_nopt op (List.map ~f:str es)
  | Curry (f, es) -> "{" ^ str f ^ "}@(" ^ str_es es ^ ")"
  | Symbolic (t, x) -> "symbolic (" ^ Type.str t ^ ", " ^ str x ^ ")"

let js (e : t) : string = failwith "missing js"

let rec vars (exp : t) : string list =
  (*returns every var used in exp*)
  match exp with
  | Var x -> [ x ]
  | UnOpt (_, e) -> vars e
  | BinOpt (_, e1, e2) -> vars e1 @ vars e2
  | TriOpt (_, e1, e2, e3) -> vars e1 @ vars e2 @ vars e3
  | NOpt (_, es) -> List.concat (List.map ~f:vars es)
  | Curry (e, es) -> List.concat (vars e :: List.map ~f:vars es)
  | _ -> []

let rec to_json (e : t) : string =
  let to_json_es es = String.concat ~sep:", " (List.map ~f:to_json es) in
  match e with
  | Val v ->
      Printf.sprintf "{ \"type\" : \"value\", \"value\" : %s }" (Val.to_json v)
  | Var x -> Printf.sprintf "{ \"type\" : \"var\", \"name\" : \"%s\"}" x
  | UnOpt (op, e) ->
      Printf.sprintf "{ \"type\" : \"unop\", \"rhs\" : %s, \"op\": %s}"
        (to_json e)
        (Operators.uopt_to_json op)
  | BinOpt (op, e1, e2) ->
      Printf.sprintf
        "{ \"type\" : \"binop\", \"lhs\" : %s, \"rhs\": %s,  \"op\": %s}"
        (to_json e1) (to_json e2)
        (Operators.bopt_to_json op)
  | TriOpt (op, e1, e2, e3) ->
      Printf.sprintf
        "{ \"type\" : \"triop\", \"arg1\" : %s, \"arg2\" : %s, \"arg3\": %s,  \
         \"op\": %s}"
        (to_json e1) (to_json e2) (to_json e3)
        (Operators.topt_to_json op)
  | NOpt (op, es) ->
      Printf.sprintf "{ \"type\" : \"nop\", \"op\": %s, \"args\" : [ %s ]}"
        (Operators.nopt_to_json op)
        (String.concat ~sep:", " (List.map ~f:to_json es))
  | Curry (f, es) ->
      Printf.sprintf
        "{ \"type\" : \"curry\", \"function:\": %s, \"args\": [ %s ]}"
        (to_json f) (to_json_es es)
  | Symbolic (t, x) ->
      Printf.sprintf
        "{ \"type\" : \"symbolic\", \"val_type\" : \"%s\", \"name\" : \"%s\" }"
        (Type.str t) (str x)
