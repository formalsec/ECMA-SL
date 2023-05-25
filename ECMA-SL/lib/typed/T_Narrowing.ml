open E_Type

let rec create_narrow_union (rts : t list) (nt : t) : t =
  match nt with
  | UnionType nts ->
      if List.mem AnyType rts then AnyType
      else if List.mem UnknownType rts then UnknownType
      else
        List.map (create_narrow_union rts) nts |> fun nts' ->
        E_Type.merge_type E_Type.merge_union_type nts'
  | ObjectType ntobj ->
      List.filter (fun rt -> T_Typing.is_typeable rt nt) rts |> fun nts' ->
      E_Type.merge_type E_Type.merge_union_type nts'
  | _ -> nt

let create_narrow_type (rt : t) (nt : t) : t =
  match (rt, nt) with
  | _, AnyType -> rt
  | AnyType, _ -> AnyType
  | UnknownType, _ -> UnknownType
  | NeverType, _ -> failwith "Typed ECMA-SL: T_Narrowing.create_nt"
  | UnionType rts, _ ->
      if List.mem AnyType rts then AnyType
      else if List.mem UnknownType rts then UnknownType
      else create_narrow_union rts nt
  | _, ObjectType ntobj -> rt
  | _ -> nt

let rec narrow_union_type (ts : t list) : t list =
  let match_wide tsrc ttar =
    match tsrc with
    | LiteralType (Val.List []) -> false
    | LiteralType Val.Null -> false
    | LiteralType _ -> E_Type.type_widening tsrc = ttar
    | _ -> false
  in
  let expand_inner_types t r =
    match t with
    | UnionType ts -> List.append (narrow_union_type ts) r
    | _ -> t :: r
  in
  let f_never t r = match t with NeverType -> r | _ -> t :: r in
  let f_unique t r = if List.mem t r then r else t :: r in
  let f_narrow ts t r = if List.exists (match_wide t) ts then r else t :: r in
  List.fold_right expand_inner_types ts [] |> fun ts ->
  List.fold_right f_never ts [] |> fun ts ->
  List.fold_right f_unique ts [] |> fun ts ->
  List.fold_right (f_narrow ts) ts [] |> fun ts ->
  if List.mem AnyType ts then [ AnyType ]
  else if List.mem UnknownType ts then [ UnknownType ]
  else ts

let type_narrowing (t : t) : t =
  match t with
  | UnionType ts -> (
      let ts' = narrow_union_type ts in
      match ts' with [] -> NeverType | e :: [] -> e | e :: r -> UnionType ts')
  | _ -> t
