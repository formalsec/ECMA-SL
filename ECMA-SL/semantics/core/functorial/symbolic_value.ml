open EslBase
open EslSyntax
module E = Encoding.Expr
module Ty = Encoding.Ty
module Value = Encoding.Value
module Symbol = Encoding.Symbol

module M = struct
  (* Ecma-sl exprs *)

  type value = E.t

  let equal (e1 : value) (e2 : value) : bool = E.equal e1 e2 [@@inline]
  let hash (e : value) = E.hash e [@@inline]
  let compare (e1 : value) (e2 : value) = compare (hash e1) (hash e2)
  let pp fmt v = E.pp fmt v [@@inline]

  let int_symbol_s (x : string) : value =
    E.mk_symbol (Symbol.mk_symbol Ty_int x)
  [@@inline]

  let mk_symbol (x : string) : value = E.(make (App (`Symbol x, []))) [@@inline]
  let mk_list (vs : value list) : value = E.(make (List vs)) [@@inline]
  let mk_tuple (fst, snd) : value = E.(make (Tuple [ fst; snd ])) [@@inline]
  let is_symbolic (v : value) : bool = E.is_symbolic v

  let func (v : value) =
    match E.view v with
    | Val (Value.Str x) -> Ok (x, [])
    | _ -> Error "Value is not a function identifier"

  module Bool = struct
    include E.Bool

    let const b = v b [@@inline]
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

  let rec eval_val : Val.t -> value =
    let open E in
    function
    | Null | Void -> assert false
    | Val.Int i -> make (Val (Int i))
    | Val.Flt f -> make (Val (Real f))
    | Val.Str s -> make (Val (Str s))
    | Val.Bool b -> Bool.v b
    | Val.Symbol x -> make (App (`Symbol x, []))
    | Val.Loc _l -> assert false
    | Val.Arr arr -> make (Array (Array.map eval_val arr))
    | Val.List lst -> make (List (List.map eval_val lst))
    | Val.Tuple lst -> make (Tuple (List.map eval_val lst))
    | Val.Byte _ -> assert false
    | Val.Type _ -> assert false
    | Val.Curry (x, vs) -> make (App (`Curry x, List.map eval_val vs))

  let eval_type : Type.t -> Ty.t = function
    | NullType -> assert false
    | IntType -> Ty_int
    | FltType -> Ty_real
    | StrType -> Ty_str
    | BoolType -> Ty_bool
    | SymbolType -> assert false
    | LocType -> assert false
    | ArrayType -> Ty_array
    | ListType -> Ty_list
    | TupleType -> Ty_tuple
    | TypeType -> assert false
    | CurryType -> assert false

  let unopt =
    let open Operator in
    function
    (* General operators *)
    | Typeof -> `App `Typeof
    (* Arithmetic operators *)
    | Neg -> `Unary Ty.Neg
    (* Bitwise operators *)
    | BitwiseNot -> `Unary Ty.Neg
    (* Logical *)
    | LogicalNot -> `Unary Ty.Neg
    (* Integer operators *)
    | IntToFloat -> `Convert Ty.Reinterpret_int
    | IntToString -> `Convert Ty.String_from_int
    | IntToFourHex -> assert false
    | OctalToDecimal -> assert false
    (* Float operators *)
    | FloatToInt -> `Convert Ty.Reinterpret_float
    | FloatToString -> `Convert Ty.ToString
    | ToInt -> assert false
    | ToInt32 -> assert false
    | ToUint16 -> assert false
    | ToUint32 -> assert false
    | IsNaN -> `Unary Ty.Is_nan
    (* String operators *)
    | StringToInt -> `Convert Ty.String_to_int
    | StringToFloat -> assert false
    | FromCharCode -> `Convert Ty.String_from_code
    | FromCharCodeU -> `Convert Ty.String_from_code
    | ToCharCode -> `Convert Ty.String_to_code
    | ToCharCodeU -> `Convert Ty.String_to_code
    | ToLowerCase -> assert false
    | ToUpperCase -> assert false
    | Trim -> `Unary Ty.Trim
    | StringLen -> `Unary Ty.Seq_length
    | StringLenU -> `Unary Ty.Seq_length
    | StringConcat -> `Binary Ty.Seq_concat
    (* Object operators *)
    | ObjectToList -> assert false
    | ObjectFields -> assert false
    (* Array operators *)
    | ArrayLen -> assert false
    (* List operators *)
    | ListToArray -> assert false
    | ListHead -> assert false
    | ListTail -> assert false
    | ListLen -> assert false
    | ListSort -> assert false
    | ListReverse -> assert false
    | ListRemoveLast -> assert false
    (* Tuple operators *)
    | TupleFirst -> assert false
    | TupleSecond -> assert false
    | TupleLen -> assert false
    (* Byte operators *)
    | FloatToByte -> assert false
    | Float32ToLEBytes -> assert false
    | Float32ToBEBytes -> assert false
    | Float64ToLEBytes -> assert false
    | Float64ToBEBytes -> assert false
    | Float32FromLEBytes -> assert false
    | Float32FromBEBytes -> assert false
    | Float64FromLEBytes -> assert false
    | Float64FromBEBytes -> assert false
    | BytesToString -> assert false
    (* Math operators *)
    | Random -> assert false
    | Abs -> `Unary Ty.Abs
    | Sqrt -> `Unary Ty.Sqrt
    | Ceil -> `Unary Ty.Ceil
    | Floor -> `Unary Ty.Floor
    | Exp -> assert false
    | Log2 -> assert false
    | LogE -> assert false
    | Log10 -> assert false
    | Sin -> assert false
    | Cos -> assert false
    | Tan -> assert false
    | Sinh -> assert false
    | Cosh -> assert false
    | Tanh -> assert false
    | Asin -> assert false
    | Acos -> assert false
    | Atan -> assert false
    (* Parse operators *)
    | Utf8Decode -> assert false
    | HexDecode -> assert false
    | ParseNumber -> assert false
    | ParseString -> assert false
    | ParseDate -> assert false

  let rec eval_expr (store : store) (e : Expr.t) : value =
    let open E in
    match e.it with
    | Val v -> eval_val v
    | Var x -> (
      match Store.find store x with
      | Some v -> v
      | None -> Log.err "cannot find var '%s'" x )
    | UnOpt (op, e) -> (
      let _e = eval_expr store e in
      match unopt op with
      | `App _ -> assert false
      | `Binary _ -> assert false
      | `Convert _ -> assert false
      | `Unary _ -> assert false )
    | BinOpt (_op, e1, e2) ->
      let _e1 = eval_expr store e1 in
      let _e2 = eval_expr store e2 in
      assert false
    | TriOpt (_op, e1, e2, e3) ->
      let _e1 = eval_expr store e1 in
      let _e2 = eval_expr store e2 in
      let _e3 = eval_expr store e3 in
      assert false
    | NOpt (_op, es) ->
      let _es = List.map (eval_expr store) es in
      assert false
    | Curry (f, es) ->
      let _f = eval_expr store f in
      let _es = List.map (eval_expr store) es in
      assert false
    | Symbolic (t, x) ->
      let t = eval_type t in
      let x =
        match view (eval_expr store x) with
        | Val (Str x) -> x
        | _ -> assert false
      in
      E.mk_symbol (Symbol.mk_symbol t x)
end

module M' : Value_intf.T = M
