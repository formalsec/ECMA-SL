let rec is_type_compatible (tref : E_Type.t) (texpr : E_Type.t) : bool =
  match (tref, texpr) with
  | E_Type.AnyType, _ -> true
  | _, E_Type.AnyType -> true
  | E_Type.UnionType tref', E_Type.UnionType texpr' ->
      List.for_all (fun t -> is_type_compatible tref t) texpr'
  | E_Type.UnionType tref', _ ->
      List.exists (fun t -> is_type_compatible t texpr) tref'
  | default -> tref = texpr

let combine_types (t1 : E_Type.t) (t2 : E_Type.t) : E_Type.t =
  (* FIXME: Add subtyping to get the most generic *)
  if is_type_compatible t1 t2 then t1
  else
    match (t1, t2) with
    | E_Type.UnionType t1', E_Type.UnionType t2' ->
        E_Type.UnionType (List.append t1' t2')
    | E_Type.UnionType t1', _ -> E_Type.UnionType (List.append t1' [ t2 ])
    | _, E_Type.UnionType t2' -> E_Type.UnionType (List.append [ t1 ] t2')
    | default -> E_Type.UnionType [ t1; t2 ]
