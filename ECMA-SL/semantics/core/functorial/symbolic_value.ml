open EslBase
open EslSyntax
open EslSyntax.Operator
module E = Smtml.Expr
module Ty = Smtml.Ty
module Value = Smtml.Value
module Symbol = Smtml.Symbol

module M = struct
  type value = E.t

  let equal (e1 : value) (e2 : value) : bool = E.equal e1 e2 [@@inline]
  let hash (e : value) = E.hash e [@@inline]
  let compare (e1 : value) (e2 : value) = compare (hash e1) (hash e2)
  let pp fmt v = E.pp fmt v [@@inline]

  let int_symbol_s (x : string) : value = E.mk_symbol (Symbol.make Ty_int x)
  [@@inline]

  let mk_symbol (x : string) : value = E.(value (App (`Op "symbol", [ Str x ])))
  [@@inline]

  let mk_list (vs : value list) : value = E.(make (List vs)) [@@inline]
  let mk_tuple (fst, snd) : value = E.(make (List [ fst; snd ])) [@@inline]
  let is_symbolic (v : value) : bool = E.is_symbolic v

  let func (v : value) =
    match E.view v with
    | Val (Value.Str x) -> Ok (x, [])
    | _ -> Error "Value is not a function identifier"

  module Bool = struct
    include E.Bool

    let const b = if b then true_ else false_
    let not_ e = not e [@@inline]
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

    let pp ppf store =
      let open Fmt in
      let iter f m =
        SMap.iter
          (fun k v -> if not @@ String.starts_with ~prefix:"__" k then f (k, v))
          m
      in
      let pp_v ppf (k, v) = fmt ppf "%s -> %a" k pp v in
      fmt ppf "{ ... %a }" (pp_iter !>";@ " iter pp_v) store
  end

  type store = Store.t

  let rec expr_type v1 =
    match E.view v1 with
    | E.Relop (_, _, _, _) -> Ty.Ty_bool
    | E.Triop (_, Ty.Ite, _, a, _) -> expr_type a
    | _ -> E.ty v1

  let eval_unop (op : Operator.unopt) =
    match op with
    | Neg -> (
      fun v ->
        let t = expr_type v in
        match t with
        | Ty_int -> E.(unop Ty_int Neg v)
        | Ty_real -> E.(unop Ty_real Neg v)
        | _ -> Log.fail "TODO:x Neg" )
    | BitwiseNot -> E.(unop Ty_int Not)
    | LogicalNot -> E.(unop Ty_bool Not)
    | IntToFloat -> E.(cvtop Ty_int Reinterpret_int)
    | IntToString -> E.(cvtop Ty_int ToString)
    | FloatToInt -> E.(cvtop Ty_real Reinterpret_float)
    | FloatToString -> E.(cvtop Ty_real ToString)
    | StringToInt -> E.(cvtop Ty_str String_to_int)
    | StringToFloat -> (
      fun v ->
        match E.view v with
        | Val (Str _) -> (
          try E.(cvtop Ty_str Ty.String_to_float) v
          with _ -> E.(value (Real nan)) )
        | _ -> Log.fail "TODO:x StringToFloat" )
    | FromCharCode -> E.(cvtop Ty_str String_from_code)
    | ToCharCode -> E.(cvtop Ty_str String_to_code)
    | StringLen -> E.(unop Ty_str Length)
    | StringConcat -> (
      fun v1 ->
        match E.view v1 with
        | E.Val (Value.List _) -> E.(naryop Ty_str Concat [ v1 ])
        | E.List lst -> E.(naryop Ty_str Concat lst)
        | _ -> Log.fail "TODO:x StringConcat" )
    | ObjectToList -> assert false
    | ObjectFields -> assert false
    | ListHead -> E.(unop Ty_list Head)
    | ListTail -> E.(unop Ty_list Tail)
    | ListLen -> E.(unop Ty_list Length)
    | ListReverse -> E.(unop Ty_list Reverse)

  let eval_binop (op : Operator.binopt) =
    match op with
    | Plus -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> E.(binop Ty_int Add v1 v2)
        | (Ty_real, Ty_real) -> E.(binop Ty_real Add v1 v2)
        | (Ty_str, Ty_str) -> E.(naryop Ty_str Concat [ v1; v2 ])
        | _ -> Log.fail "TODO:x Plus" )
    | Minus -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> E.(binop Ty_int Sub v1 v2)
        | (Ty_real, Ty_real) -> E.(binop Ty_real Sub v1 v2)
        | _ -> Log.fail "TODO:x Minus" )
    | Times -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> E.(binop Ty_int Mul v1 v2)
        | (Ty_real, Ty_real) -> E.(binop Ty_real Mul v1 v2)
        | _ -> Log.fail "TODO:x Times" )
    | Div -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> E.(binop Ty_int Div v1 v2)
        | (Ty_real, Ty_real) -> E.(binop Ty_real Div v1 v2)
        | _ -> Log.fail "TODO:x Div" )
    | Modulo -> E.(binop Ty_real Rem)
    | Pow -> E.(binop Ty_real Pow)
    | BitwiseAnd -> E.(binop Ty_int And)
    | BitwiseOr -> E.(binop Ty_int Or)
    | BitwiseXor -> E.(binop Ty_int Xor)
    | ShiftLeft -> E.(binop Ty_int Shl)
    | ShiftRight -> E.(binop Ty_int ShrA)
    | ShiftRightLogical -> E.(binop Ty_int ShrL)
    | LogicalAnd -> E.(binop Ty_bool And)
    | LogicalOr -> E.(binop Ty_bool Or)
    | SCLogicalAnd -> assert false
    | SCLogicalOr -> assert false
    | Eq -> E.(relop Ty_bool Eq)
    | Ne -> E.(relop Ty_bool Ne)
    | Lt -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> E.(relop Ty_int Lt v1 v2)
        | (Ty_real, Ty_real) -> E.(relop Ty_real Lt v1 v2)
        | (Ty_str, Ty_str) -> E.(relop Ty_str Lt v1 v2)
        | _ -> Log.fail "TODO:x Lt" )
    | Le -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> E.(relop Ty_int Le v1 v2)
        | (Ty_real, Ty_real) -> E.(relop Ty_real Le v1 v2)
        | (Ty_str, Ty_str) -> E.(relop Ty_str Le v1 v2)
        | _ -> Log.fail "TODO:x Le" )
    | Gt -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> E.(relop Ty_int Gt v1 v2)
        | (Ty_real, Ty_real) -> E.(relop Ty_real Gt v1 v2)
        | (Ty_str, Ty_str) -> E.(relop Ty_str Gt v1 v2)
        | _ -> Log.fail "TODO:x Gt" )
    | Ge -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> E.(relop Ty_int Ge v1 v2)
        | (Ty_real, Ty_real) -> E.(relop Ty_real Ge v1 v2)
        | (Ty_str, Ty_str) -> E.(relop Ty_str Ge v1 v2)
        | _ -> Log.fail "TODO:x Ge" )
    | ObjectMem -> assert false
    | StringNth -> E.(binop Ty_str At)
    | ListNth -> E.(binop Ty_list At)
    | ListAdd -> E.(binop Ty_list List_append_last)
    | ListPrepend -> (
      fun v1 v2 ->
        match expr_type v2 with
        | Ty_list -> E.(binop Ty_list Ty.List_append) v2 v1
        | _ -> Log.fail "TODO:x ListPrepend" )
    | ListConcat -> (
      fun v1 v2 ->
        match (E.view v1, E.view v2) with
        | (E.List l1, E.List l2) -> E.(naryop Ty_list Concat (l1 @ l2))
        | _ -> Log.fail "TODO:x ListConcat" )

  let eval_triop (op : Operator.triopt) =
    match op with
    | Conditional -> E.(triop Ty_bool Ite)
    | StringSubstr -> E.(triop Ty_str String_extract)
    | ListSet -> E.(triop Ty_list List_set)

  let eval_nop (op : Operator.nopt) =
    match op with
    | NAryLogicalAnd -> E.(naryop Ty_bool Logand)
    | NAryLogicalOr -> E.(naryop Ty_bool Logor)
    | ListExpr ->
      (* TODO:x to check if this is right *)
      fun vs -> E.(make (List vs))

  let rec eval_expr (store : store) (e : Expr.t) : value =
    match e.it with
    | Val v -> E.value v
    | Var x -> (
      match Store.find store x with
      | Some v -> v
      | None -> Log.fail "Cannot find var '%s'" x )
    | UnOpt (op, e) ->
      let e' = eval_expr store e in
      eval_unop op e'
    | BinOpt (op, e1, e2) ->
      let e1' = eval_expr store e1 in
      let e2' = eval_expr store e2 in
      eval_binop op e1' e2'
    | TriOpt (op, e1, e2, e3) ->
      let e1' = eval_expr store e1 in
      let e2' = eval_expr store e2 in
      let e3' = eval_expr store e3 in
      eval_triop op e1' e2' e3'
    | NOpt (op, es) ->
      let es' = List.map (eval_expr store) es in
      eval_nop op es'
    | Curry (f, es) -> (
      let f' = eval_expr store f in
      let es' = List.map (eval_expr store) es in
      match E.view f' with
      | Val (Value.Str f') -> E.(make (App (`Op f', es')))
      | _ -> Log.fail "error" )
    | Symbolic (t, x) -> (
      let x' = eval_expr store x in
      match E.view x' with
      | Val (Value.Str x') -> E.(make (Symbol (Symbol.make t x')))
      | _ -> Log.fail "error" )
end

module M' : Value_intf.T = M
