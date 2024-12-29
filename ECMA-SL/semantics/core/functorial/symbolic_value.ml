open EslBase
open EslSyntax
open EslSyntax.Operator
module Ty = Smtml.Ty
module Value = Smtml.Value
module Symbol = Smtml.Symbol

module M = struct
  type value = Smtml.Expr.t

  let equal (e1 : value) (e2 : value) : bool = Smtml.Expr.equal e1 e2 [@@inline]
  let hash (e : value) = Smtml.Expr.hash e [@@inline]
  let compare (e1 : value) (e2 : value) = compare (hash e1) (hash e2)
  let pp fmt v = Smtml.Expr.pp fmt v [@@inline]
  let to_string v = Fmt.str "%a" pp v

  let int_symbol_s (x : string) : value =
    Smtml.Expr.symbol (Symbol.make Ty_int x)
  [@@inline]

  let mk_symbol (x : string) : value =
    Smtml.Expr.value (App (`Op "symbol", [ Str x ]))
  [@@inline]

  let mk_list (vs : value list) : value = Smtml.Expr.make (List vs) [@@inline]

  let mk_tuple (fst, snd) : value = Smtml.Expr.make (List [ fst; snd ])
  [@@inline]

  let is_symbolic (v : value) : bool = Smtml.Expr.is_symbolic v

  let func (v : value) =
    match Smtml.Expr.view v with
    | Val (Value.Str x) -> Ok (x, [])
    | _ -> Error "Value is not a function identifier"

  module Bool = struct
    include Smtml.Expr.Bool

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
          (fun k v -> f (k, v) )
            (* if not @@ String.starts_with ~prefix:"__" k then f (k, v)) *)
          m
      in
      let pp_v ppf (k, v) = Fmt.pf ppf "@[<hov 1>%s@ ->@ %a@]" k pp v in
      Fmt.pf ppf "@[<hov 1>{ %a }@]" (Fmt.iter ~sep:semi iter pp_v) store
  end

  type store = Store.t

  let rec expr_type v1 =
    match Smtml.Expr.view v1 with
    | Smtml.Expr.Relop (_, _, _, _) -> Ty.Ty_bool
    | Smtml.Expr.Triop (_, Ty.Ite, _, a, _) -> expr_type a
    | _ -> Smtml.Expr.ty v1

  let eval_unop (op : Operator.unopt) =
    match op with
    | Neg -> (
      fun v ->
        let t = expr_type v in
        match t with
        | Ty_int -> Smtml.Expr.unop Ty_int Neg v
        | Ty_real -> Smtml.Expr.unop Ty_real Neg v
        | _ -> Log.fail "TODO:x Neg" )
    | BitwiseNot -> Smtml.Expr.unop Ty_int Not
    | LogicalNot -> Smtml.Expr.unop Ty_bool Not
    | ListHead -> Smtml.Expr.unop Ty_list Head
    | ListTail -> Smtml.Expr.unop Ty_list Tail
    | Typeof -> (
      fun v ->
        match Smtml.Expr.view v with
        | Val v' ->
          Smtml.Expr.value (Eval_op.typeof_semantics (v', Source.none))
        | Relop _ -> Smtml.Expr.value (Str "bool")
        | Cvtop (Ty_real, ToString, _) -> Smtml.Expr.value (Str "string")
        | Cvtop (Ty_str, String_to_float, _) -> Smtml.Expr.value (Str "float")
        | _ -> (
          match Smtml.Expr.ty v with
          | Ty_int -> Smtml.Expr.value (Str "int")
          | Ty_real -> Smtml.Expr.value (Str "float")
          | Ty_bool -> Smtml.Expr.value (Str "bool")
          | Ty_str -> Smtml.Expr.value (Str "string")
          | Ty_list -> Smtml.Expr.value (Str "list")
          | Ty_app -> Smtml.Expr.value (Str "app")
          | _ -> Log.fail "Typeof unknown value: %a" Smtml.Expr.pp v ) )
    | IntToFloat -> Smtml.Expr.cvtop Ty_real Reinterpret_int
    | IntToString -> Smtml.Expr.cvtop Ty_int ToString
    | FloatToInt -> Smtml.Expr.cvtop Ty_real Reinterpret_float
    | FloatToString -> Smtml.Expr.cvtop Ty_real ToString
    | StringToInt -> Smtml.Expr.cvtop Ty_str String_to_int
    | StringToFloat -> (
      fun v ->
        try Smtml.Expr.cvtop Ty_str String_to_float v
        with _ -> Smtml.Expr.value (Real Float.nan) )
    | ObjectToList -> assert false
    | ObjectFields -> assert false

  let eval_binop (op : Operator.binopt) =
    match op with
    | Plus -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> Smtml.Expr.binop Ty_int Add v1 v2
        | (Ty_real, Ty_real) -> Smtml.Expr.binop Ty_real Add v1 v2
        | (Ty_str, Ty_str) -> Smtml.Expr.naryop Ty_str Concat [ v1; v2 ]
        | (Ty_str, _) | (_, Ty_str) ->
          Smtml.Expr.naryop Ty_str Concat [ v1; v2 ]
        | _ ->
          Log.fail "TODO: (plus (%a : %a) (%a : %a))" Smtml.Expr.pp v1 Ty.pp t1
            Smtml.Expr.pp v2 Ty.pp t2 )
    | Minus -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> Smtml.Expr.binop Ty_int Sub v1 v2
        | (Ty_real, Ty_real) -> Smtml.Expr.binop Ty_real Sub v1 v2
        | _ -> Log.fail "TODO:x Minus" )
    | Times -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> Smtml.Expr.binop Ty_int Mul v1 v2
        | (Ty_real, Ty_real) -> Smtml.Expr.binop Ty_real Mul v1 v2
        | _ -> Log.fail "TODO:x Times" )
    | Div -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> Smtml.Expr.binop Ty_int Div v1 v2
        | (Ty_real, Ty_real) -> Smtml.Expr.binop Ty_real Div v1 v2
        | _ -> Log.fail "TODO:x Div %a %a" Smtml.Expr.pp v1 Smtml.Expr.pp v2 )
    | Modulo -> (
      fun v1 v2 ->
        match (expr_type v1, expr_type v2) with
        | (Ty_int, Ty_int) -> Smtml.Expr.binop Ty_int Rem v1 v2
        | (Ty_real, Ty_real) ->
          let v1 = Smtml.Expr.cvtop Ty_int Reinterpret_float v1 in
          let v2 = Smtml.Expr.cvtop Ty_int Reinterpret_float v2 in
          Smtml.Expr.cvtop Ty_real Reinterpret_int
            (Smtml.Expr.binop Ty_int Rem v1 v2)
        | _ -> assert false )
    | Pow -> Smtml.Expr.binop Ty_real Pow
    | BitwiseAnd -> Smtml.Expr.binop Ty_int And
    | BitwiseOr -> Smtml.Expr.binop Ty_int Or
    | BitwiseXor -> Smtml.Expr.binop Ty_int Xor
    | ShiftLeft -> Smtml.Expr.binop Ty_int Shl
    | ShiftRight -> Smtml.Expr.binop Ty_int ShrA
    | ShiftRightLogical -> Smtml.Expr.binop Ty_int ShrL
    | LogicalAnd -> Smtml.Expr.binop Ty_bool And
    | LogicalOr -> Smtml.Expr.binop Ty_bool Or
    | SCLogicalAnd -> assert false
    | SCLogicalOr -> assert false
    | Eq -> (
      fun v1 v2 ->
        match (expr_type v1, expr_type v2) with
        | (Ty_real, Ty_real) -> Smtml.Expr.relop Ty_real Eq v1 v2
        | _ -> Smtml.Expr.relop Ty_bool Eq v1 v2 )
    | Ne -> (
      fun v1 v2 ->
        match (expr_type v1, expr_type v2) with
        | (Ty_real, Ty_real) -> Smtml.Expr.relop Ty_real Ne v1 v2
        | _ -> Smtml.Expr.relop Ty_bool Ne v1 v2 )
    | Lt -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> Smtml.Expr.relop Ty_int Lt v1 v2
        | (Ty_real, Ty_real) -> Smtml.Expr.relop Ty_real Lt v1 v2
        | (Ty_str, Ty_str) -> Smtml.Expr.relop Ty_str Lt v1 v2
        | _ -> Smtml.Expr.relop Ty_int Lt v1 v2 )
    | Le -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> Smtml.Expr.relop Ty_int Le v1 v2
        | (Ty_real, Ty_real) -> Smtml.Expr.relop Ty_real Le v1 v2
        | (Ty_str, Ty_str) -> Smtml.Expr.relop Ty_str Le v1 v2
        | _ -> Log.fail "TODO:x Le" )
    | Gt -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> Smtml.Expr.relop Ty_int Gt v1 v2
        | (Ty_real, Ty_real) -> Smtml.Expr.relop Ty_real Gt v1 v2
        | (Ty_str, Ty_str) -> Smtml.Expr.relop Ty_str Gt v1 v2
        | _ -> Log.fail "TODO:x Gt" )
    | Ge -> (
      fun v1 v2 ->
        let (t1, t2) = (expr_type v1, expr_type v2) in
        match (t1, t2) with
        | (Ty_int, Ty_int) -> Smtml.Expr.relop Ty_int Ge v1 v2
        | (Ty_real, Ty_real) -> Smtml.Expr.relop Ty_real Ge v1 v2
        | (Ty_str, Ty_str) -> Smtml.Expr.relop Ty_str Ge v1 v2
        | _ -> Log.fail "TODO:x Ge" )
    | ObjectMem -> assert false

  let eval_triop (op : Operator.triopt) =
    match op with Conditional -> Smtml.Expr.(triop Ty_bool Ite)

  let eval_nop (op : Operator.nopt) =
    match op with
    | NAryLogicalAnd -> Smtml.Expr.naryop Ty_bool Logand
    | NAryLogicalOr -> Smtml.Expr.naryop Ty_bool Logor
    | ListExpr ->
      (* TODO:x to check if this is right *)
      fun vs -> Smtml.Expr.make (List vs)

  let rec eval_expr (store : store) (e : Expr.t) : value =
    match e.it with
    | Val v -> Smtml.Expr.value v
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
      match Smtml.Expr.view f' with
      | Val (Value.Str f') ->
        Smtml.Expr.make (App (Smtml.Symbol.(mk term f'), es'))
      | _ -> Log.fail "error" )
end

module M' : Value_intf.T = M
