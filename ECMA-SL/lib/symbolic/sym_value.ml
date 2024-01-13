let ( let* ) o f = Result.bind o f
let ( let+ ) o f = Result.map f o
let return = Result.ok

let list_map ~f l =
  let exception E of string in
  try
    return
    @@ List.map (fun v -> match f v with Error s -> raise (E s) | Ok v -> v) l
  with E s -> Error s

module M = struct
  module E = Encoding.Expr
  module V = Encoding.Value
  module T = Encoding.Ty

  type value = E.t

  let ( @: ) = E.( @: )
  (* let mk_symbol s = E.mk_symbol s *)
  let mk_symbol _s = assert false [@@inline]
  let mk_list (_vs : value list) : value = assert false [@@inline]
  let mk_tuple (_fst, _snd) : value = assert false [@@inline]
  
  let rec is_symbolic (v : value) : bool =
    let open E in
      match v.e with
      | Val _ -> false
      | Symbol _ -> true
      | Unop (_, v) -> is_symbolic v
      | Binop (_, v1, v2) -> is_symbolic v1 || is_symbolic v2
      | Triop (_, v1, v2, v3) -> List.exists is_symbolic [ v1; v2; v3 ]
      | Cvtop (_, v) -> is_symbolic v
      | Relop (_, v1, v2) -> is_symbolic v1 || is_symbolic v2
      | List vs | Tuple vs -> List.exists is_symbolic vs
      | Array vs -> Array.exists is_symbolic vs
      | App (_, vs) -> List.exists is_symbolic vs
      | _ -> assert false
  
  let equal (e1 : value) (e2 : value) = E.equal e1 e2

  let get_func_name (v : value) =
    let open E in
      match v.e with
      | E.Val (V.Str x) -> Ok (x, [])
      (* | Curry (Val (Val.Str x), vs) -> Ok (x, vs) *)
      | _ -> Error "Value is not a function identifier"

  module Bool = struct

    let const = function
      | true -> E.Val V.True @: T.Ty_bool
      | false -> E.Val V.False @: T.Ty_bool [@@inline]
    let not_ e = E.Unop (T.Not, e) @: T.Ty_bool  [@@inline]
    let and_ e1 e2 = E.Binop (T.And, e1, e2) @: T.Ty_bool [@@inline]
    let or_ e1 e2 = E.Binop (T.Or, e1, e2) @: T.Ty_bool [@@inline]
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
  end

  type store = Store.t

  let rec eval_value (v : Val.t) : E.t =
    match v with
    | Val.Int n -> E.Val (V.Int n) @: T.Ty_int
    | Val.Str s -> E.Val (V.Str s) @: T.Ty_str
    | Val.Bool true -> E.Val (V.True) @: T.Ty_bool 
    | Val.Bool false -> E.Val (V.False) @: T.Ty_bool
    | Val.Byte n -> E.Val (V.Int n) @: T.Ty_int
    | Val.List l -> E.List (List.map eval_value l) @: T.Ty_list
    | Val.Tuple t -> E.Tuple (List.map eval_value t) @: T.Ty_tuple
    | Val.Arr a -> E.Array (Array.map eval_value a) @: T.Ty_array
    | Val.Curry (s, es) -> E.App (s, List.map eval_value es) @: T.Ty_int
    | _ -> assert false
  
  let eval_unop_operator op =
    let open Operators in
    match op with
    (* UNOP *)
    | Neg -> (Some T.Neg, None, None)
    | Not -> (Some T.Not, None, None)
    | Abs -> (Some T.Abs, None, None)
    | Floor -> (Some T.Floor, None, None)
    | IsNaN -> (Some T.Is_nan, None, None)
    | Sqrt -> (Some T.Sqrt, None, None)
    | Ceil -> (Some T.Ceil, None, None)
    (* CVTOP *)
    | IntToString | FloatToString | BytesToString -> (None, Some T.ToString, None)
    | IntOfString | FloatOfString -> (None, Some T.OfString, None)
    (* APP *)
    | _ -> (None, None, Some (str_of_unopt op))

  let eval_binop_operator op = 
    let open Operators in
    match op with
    (* BINOP *)
    | Plus -> (Some T.Add, None, None)
    | Minus -> (Some T.Sub, None, None)
    | Times -> (Some T.Mul, None, None)
    | Div -> (Some T.Div, None, None)
    | ShiftLeft -> (Some T.Shl, None, None)
    | ShiftRight -> (Some T.ShrA, None, None)
    | ShiftRightLogical -> (Some T.ShrL, None, None)
    | Log_And -> (Some T.And, None, None)
    | Log_Or -> (Some T.Or, None, None)
    | Pow -> (Some T.Pow, None, None)
    | Min -> (Some T.Min, None, None)
    | Max -> (Some T.Max, None, None)
    | LRem -> (Some T.Rem, None, None)
    (* RELOPS *)
    | Eq -> (None, Some T.Eq, None)
    | Lt -> (None, Some T.Lt, None)
    | Gt -> (None, Some T.Gt, None)
    | Le -> (None, Some T.Le, None)
    | Ge -> (None, Some T.Ge, None)
    (* APP *)
    | _ -> (None, None, Some (str_of_binopt_single op))
  
  let eval_triopt_operator op =
    let open Operators in
    match op with
    | ITE -> (Some T.Ite, None)
    | Ssubstr | SsubstrU -> (Some T.Substr, None)
    | _ -> (None, Some (str_of_triopt_single op))

  let type_translation = function
    | Type.IntType -> T.Ty_int
    | Type.FltType -> T.Ty_fp T.S32
    | Type.BoolType -> T.Ty_bool
    | Type.StrType -> T.Ty_str
    | Type.ListType -> T.Ty_list
    | Type.TupleType -> T.Ty_tuple
    | Type.ArrayType -> T.Ty_array
    | _ -> assert false

  type a = Val.t

  let rec eval_expr (store : store) (e : Expr.t) : (value, string) Result.t =
    match e with
    | Expr.Val v -> 
      return (eval_value v)
    | Expr.Var x -> (
      match Store.find store x with
      | Some v -> return v
      | None -> Error (Format.sprintf "Cannot find var '%s'" x) )
    | Expr.UnOpt (op, e) -> (
      let op' = eval_unop_operator op in
      let+ e' = eval_expr store e in
      match op' with
      | Some op', _, _ -> E.Unop (op', e') @: e'.E.ty
      | _, Some op', _ -> E.Cvtop (op', e') @: e'.E.ty
      | _, _, Some op' -> E.App (op', [ e' ]) @: e'.E.ty
      | _ -> assert false )
    | Expr.BinOpt (op, e1, e2) -> (
      let op' = eval_binop_operator op in
      let* e1' = eval_expr store e1 in
      let+ e2' = eval_expr store e2 in
      match op' with
      | Some op', _, _ -> E.Binop (op', e1', e2') @: e1'.E.ty
      | _, Some op', _ -> E.Relop (op', e1', e2') @: e1'.E.ty
      | _, _, Some op' -> E.App (op', [ e1'; e2' ]) @: e1'.E.ty
      | _ -> assert false )
    | Expr.TriOpt (op, e1, e2, e3) -> (
      let op' = eval_triopt_operator op in
      let* e1' = eval_expr store e1 in
      let* e2' = eval_expr store e2 in
      let+ e3' = eval_expr store e3 in
      match op' with
      | Some op', _ -> E.Triop (op', e1', e2', e3') @: e1'.E.ty
      | _, Some op' -> E.App (op', [ e1'; e2'; e3' ]) @: e1'.E.ty 
      | _ -> assert false ) 
    | Expr.NOpt (op, es) ->
      let+ es' = list_map ~f:(eval_expr store) es in
      let op' = Operators.str_of_nopt_single op in
      let fst = List.hd es' in
      E.App (op', es') @: fst.E.ty
    | Expr.Curry (f, es) ->
      let* f' = eval_expr store f in
      let s = match f'.E.e with E.Val (V.Str v) -> v | _ -> assert false in
      let+ es' = list_map ~f:(eval_expr store) es in
      let fst = List.hd es' in
      E.App (s, es') @: fst.E.ty
    | Expr.Symbolic (t, x) ->
      let+ x' = eval_expr store x in
      let t' = type_translation t in
      let s = match x'.E.e with E.Val (V.Str v) -> v | _ -> assert false in
      E.mk_symbol (Encoding.Symbol.mk_symbol t' s)

  module Pp = struct
    let rec pp (e : value) : string =
      let concat es = String.concat ", " (List.map pp es) in
      match e with
      | Val n -> Val.str n
      | UnOpt (op, e) -> Operators.str_of_unopt op ^ "(" ^ pp e ^ ")"
      | BinOpt (op, e1, e2) -> Operators.str_of_binopt op (pp e1) (pp e2)
      | TriOpt (op, e1, e2, e3) ->
        Operators.str_of_triopt op (pp e1) (pp e2) (pp e3)
      | NOpt (op, es) -> Operators.str_of_nopt op (List.map pp es)
      | Curry (f, es) -> "{" ^ pp f ^ "}@(" ^ concat es ^ ")"
      | Symbolic (_t, x) -> (
        match x with Val (Val.Str x) -> x | _ -> assert false )

    module Store = struct
      type t = Store.t

      module SMap = Store.SMap

      let to_string (store : t) : string =
        let start = "{ ... " in
        SMap.fold
          (fun key data acc ->
            if String.starts_with ~prefix:"__" key then acc
            else Printf.sprintf "%s; %s -> %s" acc key (pp data) )
          store start
        ^ " }"
    end
  end
end

module M' : Value_intf.T = M
