open E_Type

let test_typing_object (is_typeable_fun : E_Type.t -> E_Type.t -> bool)
    (tref : E_Type.t) (texpr : E_Type.t) : unit =
  match (tref, texpr) with
  | E_Type.ObjectType oref, E_Type.ObjectType oexpr ->
      Hashtbl.iter
        (fun fn ft ->
          match Hashtbl.find_opt oexpr.flds fn with
          | None -> if not ft.opt then T_Err.raise (T_Err.MissingField fn)
          | Some texpr' ->
              if not (is_typeable_fun ft.t texpr'.t) then
                T_Err.raise (T_Err.BadField (fn, ft.t, texpr'.t)))
        oref.flds;
      Hashtbl.iter
        (fun fn _ ->
          match Hashtbl.find_opt oref.flds fn with
          | None -> T_Err.raise (T_Err.ExtraField fn)
          | Some _ -> ())
        oexpr.flds
  | default -> ()

let test_typing_union (is_typeable_fun : E_Type.t -> E_Type.t -> bool)
    (tref : E_Type.t) (texpr : E_Type.t) : unit =
  let test_typing_union' () =
    match (tref, texpr) with
    | E_Type.UnionType tref', E_Type.UnionType texpr' ->
        List.for_all (fun t -> is_typeable_fun tref t) texpr'
    | E_Type.UnionType tref', _ ->
        List.exists (fun t -> is_typeable_fun t texpr) tref'
    | default -> false
  in
  if not (test_typing_union' ()) then
    T_Err.raise (T_Err.BadExpectedType (tref, texpr))

let rec test_typing ?(subtyping : bool = true) (tref : E_Type.t)
    (texpr : E_Type.t) : unit =
  match (tref, texpr) with
  | _, E_Type.AnyType -> ()
  | E_Type.AnyType, _ -> ()
  | E_Type.UnknownType, _ -> ()
  | E_Type.UndefinedType, E_Type.UndefinedType -> ()
  | E_Type.NumberType, E_Type.NumberType -> ()
  | E_Type.StringType, E_Type.StringType -> ()
  | E_Type.BooleanType, E_Type.BooleanType -> ()
  | E_Type.SymbolType, E_Type.SymbolType -> ()
  | E_Type.ObjectType _, _ ->
      test_typing_object (is_typeable ~subtyping:false) tref texpr
  | E_Type.UnionType _, _ ->
      test_typing_union (is_typeable ~subtyping) tref texpr
  | default -> T_Err.raise (T_Err.BadExpectedType (tref, texpr))

and is_typeable ?(subtyping : bool = true) (tref : E_Type.t) (texpr : E_Type.t)
    : bool =
  try
    let _ = test_typing ~subtyping tref texpr in
    true
  with T_Err.TypeError _ -> false

let test_typing_expr ?(subtyping : bool = true)
    ?(gerr_fun : (unit -> T_Err.err) option = None) (tref : E_Type.t)
    (expr : E_Expr.t) (texpr : E_Type.t) : unit =
  try test_typing ~subtyping tref texpr
  with T_Err.TypeError terr -> (
    match (T_Err.get_err terr, expr) with
    | T_Err.MissingField fn, E_Expr.NewObj _ ->
        T_Err.raise (T_Err.MissingField fn) ~cs:(T_Err.Expr expr)
    | T_Err.ExtraField fn, E_Expr.NewObj fes ->
        let fe = List.find (fun (fe, _) -> fn = fe) fes in
        T_Err.raise (T_Err.ExtraField fn) ~cs:(T_Err.Str (fst fe))
    | T_Err.BadField (fn, ft, texpr), E_Expr.NewObj fes ->
        let fe = List.find (fun (fe, _) -> fn = fe) fes in
        T_Err.raise (T_Err.BadField (fn, ft, texpr)) ~cs:(T_Err.Expr (snd fe))
    | default -> (
        match gerr_fun with
        | None -> Caml.raise (T_Err.TypeError terr)
        | Some gerr_fun' -> T_Err.raise (gerr_fun' ()) ~cs:(T_Err.Expr expr)))

let intersect_types (t1 : E_Type.t) (t2 : E_Type.t) : E_Type.t =
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

let intersect_opt_types (t1 : E_Type.t option) (t2 : E_Type.t option) :
    E_Type.t option =
  match (t1, t2) with
  | Some t1', Some t2' -> Some (intersect_types t1' t2')
  | default -> None
