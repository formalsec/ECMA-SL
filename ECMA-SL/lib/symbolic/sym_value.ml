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

  (* let int_symbol (x : value) : value = Symbolic (Type.IntType, x) [@@inline] *)

  (* let int_symbol_s (x : string) : value = int_symbol (Val (Val.Str x)) *)
  (* [@@inline] *)
  
  let ( @: ) = E.( @: )

  let mk_symbol s = E.mk_symbol s

  let mk_list (_vs : value list) : value = assert false
  (* let mk_list (vs : value list) : value = NOpt (Operators.ListExpr, vs)
  [@@inline] *)

  let mk_tuple (_fst, _snd) : value = assert false
  (* let mk_tuple (fst, snd) : value = NOpt (Operators.TupleExpr, [ fst; snd ])
  [@@inline] *)
  
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
      | _ -> assert false
      (* | NOpt (_, es) | Curry (_, es) ->
        (not (List.is_empty es)) && List.exists is_symbolic es *)
  
  let equal (e1 : value) (e2 : value) = E.equal e1 e2

  let get_func_name (v : value) =
    let open E in
      match v.e with
      | E.Val (V.Str x) -> Ok (x, [])
      (* | Curry (Val (Val.Str x), vs) -> Ok (x, vs) *)
      | _ -> Error "Value is not a function identifier"

  module Bool = struct

    let const b = E.Val b @: T.Ty_bool [@@inline]
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

  (* let eval_value (v : Val.t) : E.t =
    match v with
    | Val.Int n -> E.Val (V.Int n)
    | Val.Bool b -> E.Val (V.Bool b)
    | Val.Str s -> E.Val (V.Str s)
    | Val.List l -> E.List List.map eval_value l
    | _ -> assert false *)

  let eval_value (v : Val.t) : E.t =
    match v with
    | Val.Int n -> E.Val (V.Int n) @: T.Ty_int
    | Val.Str s -> E.Val (V.Str s) @: T.Ty_str
    | Val.Bool b -> match b with true -> E.Val (V.True) @: T.Ty_bool | false -> E.Val (V.False) @: T.Ty_bool
    | _ -> assert false
  

  let rec eval_expr (store : store) (e : Expr.t) : (value, string) Result.t =
    match e with
    | Expr.Val v -> 
      return (eval_value v)
    | Expr.Var x -> (
      match Store.find store x with
      | Some v -> return v
      | None -> Error (Format.sprintf "Cannot find var '%s'" x) )
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
