open Core

let ( let* ) o f = Result.bind o ~f
let ( let+ ) o f = Result.map o ~f
let return = Result.return

let list_map ~f l =
  let exception E of string in
  try
    return
    @@ List.map l ~f:(fun v ->
         match f v with Error s -> raise (E s) | Ok v -> v )
  with E s -> Error s

module M = struct
  type value =
    | Val of Val.t
    | UnOpt of (Operators.uopt * value)
    | BinOpt of (Operators.bopt * value * value)
    | TriOpt of (Operators.topt * value * value * value)
    | NOpt of Operators.nopt * value list
    | Curry of value * value list
    | Symbolic of Type.t * value

  let get_func_name (v : value) =
    match v with
    | Val (Val.Str x) -> Ok (x, [])
    | Curry (Val (Val.Str x), vs) -> Ok (x, vs)
    | _ -> Error "Value is not a function identifier"

  let rec is_symbolic (v : value) : bool =
    match v with
    | Val _ -> false
    | Symbolic _ -> true
    | UnOpt (_, v) -> is_symbolic v
    | BinOpt (_, v1, v2) -> is_symbolic v1 || is_symbolic v2
    | TriOpt (_, v1, v2, v3) -> List.exists ~f:is_symbolic [ v1; v2; v3 ]
    | NOpt (_, es) | Curry (_, es) ->
      (not (List.is_empty es)) && List.exists es ~f:is_symbolic

  let rec equal (e1 : value) (e2 : value) : bool =
    match (e1, e2) with
    | Val v1, Val v2 -> Val.equal v1 v2
    | Symbolic (t1, e1'), Symbolic (t2, e2') -> Type.(t1 = t2) && equal e1' e2'
    | UnOpt (op1, e1'), UnOpt (op2, e2') ->
      Stdlib.( = ) op1 op2 && equal e1' e2'
    | BinOpt (op1, e1', e2'), BinOpt (op2, e3', e4') ->
      Stdlib.( = ) op1 op2 && equal e1' e3' && equal e2' e4'
    | TriOpt (op1, e1', e2', e3'), TriOpt (op2, e4', e5', e6') ->
      Stdlib.( = ) op1 op2 && equal e1' e4' && equal e2' e5' && equal e3' e6'
    | NOpt (op1, es1), NOpt (op2, es2) ->
      Stdlib.( = ) op1 op2 && List.equal equal es1 es2
    | Curry (x1, es1), Curry (x2, es2) ->
      equal x1 x2 && List.equal equal es1 es2
    | _ -> false

  module Bool = struct
    let const b = Val (Val.Bool b) [@@inline]
    let not_ e = UnOpt (Operators.Not, e) [@@inline]
    let and_ e1 e2 = BinOpt (Operators.Log_And, e1, e2) [@@inline]
    let or_ e1 e2 = BinOpt (Operators.Log_Or, e1, e2) [@@inline]
  end

  module Store = struct
    module SMap = Map.Make (String)

    type bind = string
    type t = value SMap.t

    let create (values : (bind * value) list) : t =
      List.fold values ~init:SMap.empty ~f:(fun accum (key, data) ->
        SMap.set accum ~key ~data )

    let mem (store : t) (x : bind) : bool = SMap.mem store x

    let add_exn (store : t) (key : bind) (data : value) : t =
      SMap.set store ~key ~data

    let find (store : t) (x : bind) : value option = SMap.find store x
  end

  type store = Store.t

  let rec eval_expr (store : store) (e : Expr.t) : (value, string) Result.t =
    match e with
    | Expr.Val v -> return (Val v)
    | Expr.Var x ->
      Result.of_option (Store.find store x)
        ~error:(sprintf "Cannot find var '%s'" x)
    | Expr.UnOpt (op, e) -> (
      let+ e' = eval_expr store e in
      match e' with Val v -> Val (Eval_op.eval_unop op v) | _ -> UnOpt (op, e')
      )
    | Expr.BinOpt (op, e1, e2) -> (
      let* e1' = eval_expr store e1 in
      let+ e2' = eval_expr store e2 in
      match (e1', e2') with
      | Val v1, Val v2 -> Val (Eval_op.eval_binopt_expr op v1 v2)
      | _ -> BinOpt (op, e1', e2') )
    | Expr.TriOpt (op, e1, e2, e3) -> (
      let* e1' = eval_expr store e1 in
      let* e2' = eval_expr store e2 in
      let+ e3' = eval_expr store e3 in
      match (e1', e2', e3') with
      | Val v1, Val v2, Val v3 -> Val (Eval_op.eval_triopt_expr op v1 v2 v3)
      | _ -> TriOpt (op, e1', e2', e3') )
    | Expr.NOpt (op, es) ->
      let+ es' = list_map ~f:(eval_expr store) es in
      NOpt (op, es')
    | Expr.Curry (f, es) ->
      let* f' = eval_expr store f in
      let+ es' = list_map ~f:(eval_expr store) es in
      Curry (f', es')
    | Expr.Symbolic (t, x) ->
      let+ x' = eval_expr store x in
      Symbolic (t, x')

  module Pp = struct
    let rec pp (e : value) : string =
      let concat es = String.concat ~sep:", " (List.map ~f:pp es) in
      match e with
      | Val n -> Val.str n
      | UnOpt (op, e) -> Operators.str_of_unopt op ^ "(" ^ pp e ^ ")"
      | BinOpt (op, e1, e2) -> Operators.str_of_binopt op (pp e1) (pp e2)
      | TriOpt (op, e1, e2, e3) ->
        Operators.str_of_triopt op (pp e1) (pp e2) (pp e3)
      | NOpt (op, es) -> Operators.str_of_nopt op (List.map ~f:pp es)
      | Curry (f, es) -> "{" ^ pp f ^ "}@(" ^ concat es ^ ")"
      | Symbolic (_t, x) -> pp x

    module Store = struct
      type t = Store.t

      module SMap = Store.SMap

      let to_string (store : t) : string =
        let start = "{ ... " in
        SMap.fold store ~init:start ~f:(fun ~key ~data accum ->
          if String.is_prefix ~prefix:"__" key then accum
          else Printf.sprintf "%s; %s -> %s" accum key (pp data) )
        ^ " }"
    end
  end
end

module M' : Value_intf.T = M
