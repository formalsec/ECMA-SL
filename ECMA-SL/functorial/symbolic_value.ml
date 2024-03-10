open EslCore

module M = struct
  type value =
    | Val of Val.t
    | UnOpt of Operator.unopt * value
    | BinOpt of Operator.binopt * value * value
    | TriOpt of Operator.triopt * value * value * value
    | NOpt of Operator.nopt * value list
    | Curry of value * value list
    | Symbolic of Type.t * value

  let int_symbol (x : value) : value = Symbolic (Type.IntType, x) [@@inline]

  let int_symbol_s (x : string) : value = int_symbol (Val (Val.Str x))
  [@@inline]

  let mk_symbol (x : string) : value = Val (Val.Symbol x) [@@inline]

  let mk_list (vs : value list) : value = NOpt (Operator.ListExpr, vs)
  [@@inline]

  let mk_tuple (fst, snd) : value = NOpt (Operator.TupleExpr, [ fst; snd ])
  [@@inline]

  let rec is_symbolic (v : value) : bool =
    match v with
    | Val _ -> false
    | Symbolic _ -> true
    | UnOpt (_, v) -> is_symbolic v
    | BinOpt (_, v1, v2) -> is_symbolic v1 || is_symbolic v2
    | TriOpt (_, v1, v2, v3) -> List.exists is_symbolic [ v1; v2; v3 ]
    | NOpt (_, es) | Curry (_, es) ->
      List.length es <> 0 && List.exists is_symbolic es

  let rec equal (e1 : value) (e2 : value) : bool =
    match (e1, e2) with
    | (Val v1, Val v2) -> Val.equal v1 v2
    | (Symbolic (t1, e1'), Symbolic (t2, e2')) ->
      Type.equal t1 t2 && equal e1' e2'
    | (UnOpt (op1, e1'), UnOpt (op2, e2')) ->
      Stdlib.( = ) op1 op2 && equal e1' e2'
    | (BinOpt (op1, e1', e2'), BinOpt (op2, e3', e4')) ->
      Stdlib.( = ) op1 op2 && equal e1' e3' && equal e2' e4'
    | (TriOpt (op1, e1', e2', e3'), TriOpt (op2, e4', e5', e6')) ->
      Stdlib.( = ) op1 op2 && equal e1' e4' && equal e2' e5' && equal e3' e6'
    | (NOpt (op1, es1), NOpt (op2, es2)) ->
      Stdlib.( = ) op1 op2 && List.equal equal es1 es2
    | (Curry (x1, es1), Curry (x2, es2)) ->
      equal x1 x2 && List.equal equal es1 es2
    | _ -> false

  (* FIXME: Proper pp *)
  let rec pp fmt =
    let open Format in
    let pp_list fmt es =
      pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp fmt es
    in
    let pp_str e = asprintf "%a" pp e in
    function
    | Val n -> Val.pp fmt n
    | UnOpt (op, e) ->
      let e = pp_str e in
      fprintf fmt "%s" (Operator.str_of_unopt Format.pp_print_string op e)
    | BinOpt (op, e1, e2) ->
      let e1 = pp_str e1 in
      let e2 = pp_str e2 in
      fprintf fmt "%s" (Operator.str_of_binopt Format.pp_print_string op e1 e2)
    | TriOpt (op, e1, e2, e3) ->
      let e1 = pp_str e1 in
      let e2 = pp_str e2 in
      let e3 = pp_str e3 in
      fprintf fmt "%s"
        (Operator.str_of_triopt Format.pp_print_string op e1 e2 e3)
    | NOpt (op, es) ->
      fprintf fmt "%s"
        (Operator.str_of_nopt Format.pp_print_string op (List.map pp_str es))
    | Curry (f, es) -> fprintf fmt "{%a}@(%a)" pp f pp_list es
    | Symbolic (t, x) -> (
      match x with
      | Val (Str x) -> fprintf fmt "(`%s : %a)" x Type.pp t
      | _ -> fprintf fmt "(`%a : %a)" pp x Type.pp t )

  let func (v : value) =
    match v with
    | Val (Val.Str x) -> Ok (x, [])
    | Curry (Val (Val.Str x), vs) -> Ok (x, vs)
    | _ -> Error "Value is not a function identifier"

  module Bool = struct
    let const b = Val (Val.Bool b) [@@inline]

    let not_ = function
      | Val (Val.Bool b) -> Val (Val.Bool (not b))
      | e -> UnOpt (Operator.LogicalNot, e)

    let and_ e1 e2 =
      match (e1, e2) with
      | (Val (Val.Bool b1), Val (Bool b2)) -> Val (Bool (b1 && b2))
      | _ -> BinOpt (Operator.LogicalAnd, e1, e2)

    let or_ e1 e2 =
      match (e1, e2) with
      | (Val (Val.Bool b1), Val (Bool b2)) -> Val (Bool (b1 || b2))
      | _ -> BinOpt (Operator.LogicalOr, e1, e2)
  end

  module Store = struct
    module SMap = Map.Make (String)

    type bind = string
    type t = value SMap.t

    let create (values : (bind * value) list) : t =
      List.fold_left
        (fun acc (key, data) -> SMap.add key data acc)
        SMap.empty values

    let mem (store : t) (x : bind) : bool = SMap.mem x store

    let add_exn (store : t) (key : bind) (data : value) : t =
      SMap.add key data store

    let find (store : t) (x : bind) : value option = SMap.find_opt x store

    let pp fmt store =
      let open Fmt in
      let pp_sep fmt () = fprintf fmt ";@ " in
      let iter f m =
        SMap.iter
          (fun k v -> if not @@ String.starts_with ~prefix:"__" k then f (k, v))
          m
      in
      let pp_v fmt (k, v) = fprintf fmt "%s -> %a" k pp v in
      fprintf fmt "{ ... %a }" (pp_iter pp_sep iter pp_v) store
  end

  type store = Store.t

  let rec eval_expr (store : store) (e : Expr.t) : value =
    match e.it with
    | Expr.Val v -> Val v
    | Expr.Var x -> (
      match Store.find store x with
      | Some v -> v
      | None -> Log.err "Cannot find var '%s'" x )
    | Expr.UnOpt (op, e) -> (
      let e' = eval_expr store e in
      match e' with
      | Val v -> Val (Eval_operator.eval_unopt op v)
      | _ -> UnOpt (op, e') )
    | Expr.BinOpt (op, e1, e2) -> (
      let e1' = eval_expr store e1 in
      let e2' = eval_expr store e2 in
      match (e1', e2') with
      | (Val v1, Val v2) -> Val (Eval_operator.eval_binopt op v1 v2)
      | _ -> BinOpt (op, e1', e2') )
    | Expr.TriOpt (op, e1, e2, e3) -> (
      let e1' = eval_expr store e1 in
      let e2' = eval_expr store e2 in
      let e3' = eval_expr store e3 in
      match (e1', e2', e3') with
      | (Val v1, Val v2, Val v3) -> Val (Eval_operator.eval_triopt op v1 v2 v3)
      | _ -> TriOpt (op, e1', e2', e3') )
    | Expr.NOpt (op, es) ->
      let es' = List.map (eval_expr store) es in
      NOpt (op, es')
    | Expr.Curry (f, es) ->
      let f' = eval_expr store f in
      let es' = List.map (eval_expr store) es in
      Curry (f', es')
    | Expr.Symbolic (t, x) ->
      let x' = eval_expr store x in
      Symbolic (t, x')
end

module M' : Value_intf.T = M
