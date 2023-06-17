open S_object

let solver = Encoding.Batch.create ()
let pc = []
let expr_TRUE = Expr.Val (Val.Bool true)
let o : Expr.t S_object.t = S_object.create ()
let store = Sstore.create []

let objects =
  S_object.set o (Expr.Val (Val.Str "key")) (Expr.Val (Val.Str "val")) solver pc
    store

let%test "empty_concrete_set" =
  List.length objects = 1
  &&
  let o, pc =
    match objects with (o, pc) :: tail -> (o, pc) | _ -> failwith "error"
  in
  let v = get_concrete_field o "key" in
  match (v, pc) with
  | Some v, [] -> Expr.equal v (Expr.Val (Val.Str "val"))
  | _ -> false

let o, new_pc =
  match objects with (o, pc) :: tail -> (o, pc) | _ -> failwith "error"

let objects' = S_object.get o (Expr.Val (Val.Str "key")) solver pc store

let%test "concrete_get_exists" =
  List.length objects' = 1
  &&
  let o, pc, v =
    match objects' with
    | (o, pc, v) :: tail -> (o, pc, v)
    | _ -> failwith "error"
  in
  match (v, pc) with
  | Some v, [] -> Expr.equal v (Expr.Val (Val.Str "val"))
  | _ -> false

let objects' = S_object.get o (Expr.Val (Val.Str "not_key")) solver pc store

let%test "concrete_get_doesnt_exist" =
  List.length objects' = 1
  &&
  let o, pc, v =
    match objects' with
    | (o, pc, v) :: tail -> (o, pc, v)
    | _ -> failwith "error"
  in
  match (v, pc) with None, [] -> true | _ -> false

let orig_symb_key = Expr.Symbolic (Type.StrType, Expr.Val (Val.Str "symb_k"))

let objects2 =
  S_object.set o orig_symb_key (Expr.Val (Val.Str "new_symb_val")) solver pc
    store

let%test "symbolic_set" =
  List.length objects2 = 2
  &&
  let o, pc, o', pc' =
    match objects2 with
    | (o, pc) :: (o', pc') :: _ -> (o, pc, o', pc')
    | _ -> failwith "error"
  in
  let v =
    S_object.get_symbolic_field o
      (Expr.Symbolic (Type.StrType, Expr.Val (Val.Str "symb_k")))
  in
  let v' = S_object.get_concrete_field o' "key" in
  let k = Expr.Symbolic (Type.StrType, Expr.Val (Val.Str "symb_k")) in
  let eq = Expr.BinOpt (Operators.Eq, k, Expr.Val (Val.Str "key")) in
  let not1 = Expr.UnOpt (Operators.Not, eq) in
  let translated_eq = Translator.translate eq in
  let translated_expr = Translator.translate not1 in
  match (v, pc, v', pc') with
  | Some v, [ p ], Some v', [ p' ] ->
      Expr.equal v (Expr.Val (Val.Str "new_symb_val"))
      && Expr.equal v' (Expr.Val (Val.Str "new_symb_val"))
      && Encoding.Expression.equal p translated_expr
      && Encoding.Expression.equal p' translated_eq
  | _ -> false

let o2, pc =
  match objects2 with
  | (o, pc') :: tail -> ( match pc' with _ -> (o, pc' @ pc))
  | _ -> failwith "error"

(* check get for symb key that exists. *)
let objects2' =
  S_object.get o2
    (Expr.Symbolic (Type.StrType, Expr.Val (Val.Str "symb_k")))
    solver pc store

let%test "symbolic_get_exists" =
  List.length objects2' = 1
  &&
  let o, pc, v =
    match objects2' with
    | (o, pc, v) :: tail -> (o, pc, v)
    | _ -> failwith "error"
  in
  match (v, pc) with
  | Some v, [] -> Expr.equal v (Expr.Val (Val.Str "new_symb_val"))
  | _ -> false

(* check get for symb key that doesn't exist. *)
let symb_key = Expr.Symbolic (Type.StrType, Expr.Val (Val.Str "another_symb_k"))
let objects2'' = S_object.get o2 symb_key solver pc store

let%test "symbolic_get_doesnt_exist" =
  List.length objects2'' = 3
  &&
  let o, pc, v, o', pc', v', o'', pc'', v'' =
    match objects2'' with
    | (o, pc, v) :: (o', pc', v') :: (o'', pc'', v'') :: _ ->
        (o, pc, v, o', pc', v', o'', pc'', v'')
    | _ -> failwith "error"
  in
  let eq1 =
    Expr.BinOpt
      ( Operators.Eq,
        symb_key,
        Expr.Symbolic (Type.StrType, Expr.Val (Val.Str "symb_k")) )
  in
  let eq2 = Expr.BinOpt (Operators.Eq, symb_key, Expr.Val (Val.Str "key")) in
  let not1 = Expr.UnOpt (Operators.Not, eq1) in
  let not2 = Expr.UnOpt (Operators.Not, eq2) in
  let eq1 = Translator.translate eq1 in
  let eq2 = Translator.translate eq2 in

  let not1 = Translator.translate not1 in
  let not2 = Translator.translate not2 in

  match (v, pc, v', pc', v'', pc'') with
  | None, [ p1; p2 ], Some v', [ p' ], Some v'', [ p'' ] ->
      Expr.equal v' (Expr.Val (Val.Str "new_symb_val"))
      && Expr.equal v'' (Expr.Val (Val.Str "val"))
      && Encoding.Expression.equal p2 not1
      && Encoding.Expression.equal p1 not2
      && Encoding.Expression.equal p' eq1
      && Encoding.Expression.equal p'' eq2
  | _ -> false

let symb_key = Expr.Symbolic (Type.StrType, Expr.Val (Val.Str "symb_k2"))
let new_val = Expr.Val (Val.Str "new_symb_val2")

(* Set another symbolic value *)
let objects3 = S_object.set o2 symb_key new_val solver pc store

let%test "set_another_symb_key" =
  List.length objects3 = 3
  &&
  let o, pc, o', pc', o'', pc'' =
    match objects3 with
    | (o, pc) :: (o', pc') :: (o'', pc'') :: _ -> (o, pc, o', pc', o'', pc'')
    | _ -> failwith "error"
  in
  let v = S_object.get_symbolic_field o symb_key in
  let v' = S_object.get_concrete_field o' "key" in
  let v'' = S_object.get_symbolic_field o'' orig_symb_key in

  let eq1 = Expr.BinOpt (Operators.Eq, symb_key, Expr.Val (Val.Str "key")) in
  let eq2 = Expr.BinOpt (Operators.Eq, symb_key, orig_symb_key) in

  let not1 = Expr.UnOpt (Operators.Not, eq1) in
  let not2 = Expr.UnOpt (Operators.Not, eq2) in

  let eq1 = Translator.translate eq1 in
  let eq2 = Translator.translate eq2 in

  let not1 = Translator.translate not1 in
  let not2 = Translator.translate not2 in

  match (v, pc, v', pc', v'', pc'') with
  | Some v, [ p1; p2 ], Some v', [ p' ], Some v'', [ p'' ] ->
      Expr.equal v new_val && Expr.equal v' new_val && Expr.equal v'' new_val
      && Encoding.Expression.equal p1 not2
      && Encoding.Expression.equal p2 not1
      && Encoding.Expression.equal p' eq1
      && Encoding.Expression.equal p'' eq2
  | _ -> false

let o3, pc =
  match objects3 with
  | (o, pc') :: tail -> ( match pc' with _ -> (o, pc' @ pc))
  | _ -> failwith "error"

let concrete_key2 = Expr.Val (Val.Str "key2")
let new_val2 = Expr.Val (Val.Str "val2")
let objects4 = S_object.set o3 concrete_key2 new_val2 solver pc store

let%test "set_another_concrete_key" =
  List.length objects4 = 3
  && List.length objects3 = 3
  &&
  let o1, pc1, o2, pc2, o3, pc3 =
    match objects4 with
    | (o1, pc1) :: (o2, pc2) :: (o3, pc3) :: _ -> (o1, pc1, o2, pc2, o3, pc3)
    | _ -> failwith "error"
  in
  let v1 = S_object.get_concrete_field o1 "key2" in
  let v2 = S_object.get_symbolic_field o2 symb_key in
  let v3 = S_object.get_symbolic_field o3 orig_symb_key in

  let eq1 = Expr.BinOpt (Operators.Eq, concrete_key2, symb_key) in
  let eq2 = Expr.BinOpt (Operators.Eq, concrete_key2, orig_symb_key) in

  let not1 = Expr.UnOpt (Operators.Not, eq1) in
  let not2 = Expr.UnOpt (Operators.Not, eq2) in

  let not1 = Translator.translate not1 in
  let not2 = Translator.translate not2 in

  let eq1 = Translator.translate eq1 in
  let eq2 = Translator.translate eq2 in

  match (v1, pc1, v2, pc2, v3, pc3) with
  | Some v1, [ p1; p1' ], None, [ p2 ], None, [ p3 ] ->
      Expr.equal v1 new_val2
      && Encoding.Expression.equal p1 not2
      && Encoding.Expression.equal p1' not1
      && Encoding.Expression.equal p2 eq1
      && Encoding.Expression.equal p3 eq2
  | _ -> false
