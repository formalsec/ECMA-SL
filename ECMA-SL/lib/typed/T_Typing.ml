let test_typing_union (is_typeable_fun : E_Type.t -> E_Type.t -> bool)
    (tref : E_Type.t) (texpr : E_Type.t) : unit =
  let test_typing_union' () =
    match (tref, texpr) with
    | E_Type.UnionType tref', E_Type.UnionType texpr' ->
        List.for_all (fun t -> is_typeable_fun tref t) texpr'
    | E_Type.UnionType tref', _ ->
        List.exists (fun t -> is_typeable_fun t texpr) tref'
    | default -> true
  in
  if not (test_typing_union' ()) then
    T_Err.raise (T_Err.BadExpectedType (tref, texpr))

let test_typing_primitive (tref : E_Type.t) (texpr : E_Type.t) : unit =
  if not (tref = texpr) then T_Err.raise (T_Err.BadExpectedType (tref, texpr))

let rec test_typing ?(allow_subtyping : bool = true) (tref : E_Type.t)
    (texpr : E_Type.t) : unit =
  let is_typeable_fun = is_typeable ~allow_subtyping in
  match (tref, texpr) with
  | _, E_Type.AnyType -> ()
  | E_Type.AnyType, _ -> ()
  | E_Type.NumberType, _ -> test_typing_primitive tref texpr
  | E_Type.StringType, _ -> test_typing_primitive tref texpr
  | E_Type.BooleanType, _ -> test_typing_primitive tref texpr
  | E_Type.UnionType _, _ -> test_typing_union is_typeable_fun tref texpr
  | default -> T_Err.raise (T_Err.BadExpectedType (tref, texpr))

and is_typeable ?(allow_subtyping : bool = true) (tref : E_Type.t)
    (texpr : E_Type.t) : bool =
  try
    let _ = test_typing ~allow_subtyping tref texpr in
    true
  with T_Err.TypeError _ -> false

let intersect_types (t1 : E_Type.t) (t2 : E_Type.t) : E_Type.t =
  if is_typeable ~allow_subtyping:false t1 t2 then t1
  else if is_typeable ~allow_subtyping:false t2 t1 then t2
  else
    match (t1, t2) with
    | E_Type.UnionType t1', E_Type.UnionType t2' ->
        let union_t = E_Type.UnionType (List.append t1' t2') in
        E_Type.simplify_type union_t
    | E_Type.UnionType t1', _ ->
        let union_t = E_Type.UnionType (List.append t1' [ t2 ]) in
        E_Type.simplify_type union_t
    | _, E_Type.UnionType t2' ->
        let union_t = E_Type.UnionType (List.append t2' [ t1 ]) in
        E_Type.simplify_type union_t
    | default -> E_Type.UnionType [ t1; t2 ]
