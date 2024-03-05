open EType

let terr_msg (congruency : bool) (tref : EType.t) (tsrc : EType.t) :
  Eslerr.tperr =
  if congruency then BadCongruency (tref, tsrc) else BadSubtyping (tref, tsrc)

let resolve_optfld (t : t) : t =
  let open Source in
  let t_undefined = UndefinedType @> no_region in
  match t.it with
  | UnionType ts when List.exists (fun t' -> t'.it = UndefinedType) ts -> t
  | UnionType ts -> UnionType (List.append ts [ t_undefined ]) @> t.at
  | UndefinedType -> t
  | _ -> UnionType [ t; t_undefined ] @> t.at

let rec type_check ?(congruency : bool = false) (tref : t) (tsrc : t) : unit =
  match (congruency, tref.it, tsrc.it) with
  | (_, _, _) when EType.equal tref tsrc -> ()
  | (_, _, AnyType) -> ()
  | (_, AnyType, _) -> ()
  | (_, ObjectType _, ObjectType _) -> check_object congruency tref tsrc
  | (_, ListType _, ListType _) -> check_list congruency tref tsrc
  | (_, TupleType _, TupleType _) -> check_tuple congruency tref tsrc
  | (true, UnionType _, UnionType _) -> check_union_congruency tref tsrc
  | (false, _, UnionType _) -> check_union_subtyping_src tref tsrc
  | (false, UnionType _, _) -> check_union_subtyping_ref tref tsrc
  | (false, UnknownType, _) -> ()
  | (false, _, NeverType) -> ()
  | (false, IntType, LiteralType (IntegerLit _)) -> ()
  | (false, FloatType, LiteralType (FloatLit _)) -> ()
  | (false, StringType, LiteralType (StringLit _)) -> ()
  | (false, BooleanType, LiteralType (BooleanLit _)) -> ()
  | (false, SymbolType, LiteralType (SymbolLit _)) -> ()
  | (_, _, _) ->
    Eslerr.(typing ~src:(ErrSrc.at tsrc) (terr_msg congruency tref tsrc))

and is_typeable ?(congruency : bool = false) (tref : t) (tsrc : t) : bool =
  try type_check ~congruency tref tsrc |> fun () -> true
  with Eslerr.Typing_error _ -> false

and type_check_werr ?(congruency : bool = false) (tref : t) (tsrc : t)
  (msg : Eslerr.tperr) : unit =
  try type_check ~congruency tref tsrc
  with Eslerr.Typing_error _ as exn -> Eslerr.(push_tp msg exn |> raise)

and check_object (congruency : bool) (tref : t) (tsrc : t) : unit =
  let check_object_type otref otsrc =
    Hashtbl.iter (check_object_fields congruency otsrc.kind otref) otsrc.flds;
    Hashtbl.iter (check_missing_fields tsrc.at congruency otsrc) otref.flds;
    check_summary_type tsrc.at congruency otref otsrc
  in
  match (tref.it, tsrc.it) with
  | (ObjectType tobjref, ObjectType tobjsrc) -> (
    try check_object_type tobjref tobjsrc
    with Eslerr.Typing_error _ as exn ->
      Eslerr.(push_tp (terr_msg congruency tref tsrc) exn |> raise) )
  | _ -> Eslerr.(internal __FUNCTION__ (Expecting "Object type"))

and check_object_fields (congruency : bool) (tobjkind : EType.tobjkind)
  (tobjref : EType.tobject) (_ : Id.t')
  ((sfn, sft, sfs) : Id.t * EType.t * EType.tfldstyle) : unit =
  let is_obj = function ObjectType _ -> true | _ -> false in
  let fld_congruency rft = Source.(tobjkind == ObjSto && not (is_obj rft.it)) in
  let fld_err msg = Eslerr.(typing ~src:(ErrSrc.at sfn) msg) in
  let tfldref = Hashtbl.find_opt tobjref.flds sfn.it in
  match (congruency, tfldref, tobjref.smry) with
  | (true, None, _) -> fld_err (ExtraField sfn)
  | (true, Some (_, rft, rfs), _) ->
    check_field_type true sfn (rft, rfs) (sft, sfs)
  | (false, None, None) -> if tobjkind = ObjLit then fld_err (ExtraField sfn)
  | (false, Some (_, rft, rfs), _) ->
    check_field_type (fld_congruency rft) sfn (rft, rfs) (sft, sfs)
  | (false, None, Some (_, rft)) ->
    let resolve_sft = function FldReq -> sft | FldOpt -> resolve_optfld sft in
    let (rft', sft') = (resolve_optfld rft, resolve_sft sfs) in
    type_check_werr ~congruency:(fld_congruency rft) rft' sft'
      (IncompatibleSummaryField sfn)

and check_field_type (congruency : bool) (fn : Id.t)
  ((rft, rfs) : EType.t * EType.tfldstyle)
  ((sft, sfs) : EType.t * EType.tfldstyle) : unit =
  let type_check' = type_check_werr ~congruency in
  match (rfs, sfs) with
  | (FldReq, FldReq) -> type_check' rft sft (IncompatibleField fn)
  | (FldOpt, FldOpt) -> type_check' rft sft (IncompatibleField fn)
  | (FldReq, FldOpt) ->
    type_check' rft (resolve_optfld sft) (IncompatibleOptionalField fn)
  | (FldOpt, FldReq) ->
    type_check' (resolve_optfld rft) sft (IncompatibleOptionalField fn)

and check_missing_fields (at : Source.region) (congruency : bool)
  (tobjsrc : EType.tobject) (_ : Id.t')
  ((rfn, _, rfs) : Id.t * EType.t * EType.tfldstyle) : unit =
  if not (Hashtbl.mem tobjsrc.flds rfn.it) then
    if congruency || tobjsrc.kind == ObjSto || rfs == FldReq then
      Eslerr.(typing ~src:(ErrSrc.region at) (MissingField rfn))

and check_summary_type (at : Source.region) (congruency : bool)
  (tobjref : EType.tobject) (tobjsrc : EType.tobject) : unit =
  let smry_err msg = Eslerr.(typing ~src:(ErrSrc.region at) msg) in
  let congruency' = congruency || tobjsrc.kind == ObjSto in
  match (tobjref.smry, tobjsrc.smry) with
  | (None, None) -> ()
  | (Some (_, ft), None) -> if congruency' then smry_err (MissingSummaryField ft)
  | (None, Some _) -> if congruency then smry_err ExtraSummaryField
  | (Some (_, rft), Some (sfn, sft)) ->
    type_check_werr ~congruency:congruency' rft sft (IncompatibleField sfn)

and check_list (congruency : bool) (tref : EType.t) (tsrc : EType.t) : unit =
  match (tref.it, tsrc.it) with
  | (ListType tref', ListType tsrc') ->
    type_check_werr ~congruency tref' tsrc' (terr_msg congruency tref tsrc)
  | _ -> Eslerr.(internal __FUNCTION__ (Expecting "List type"))

and check_tuple (congruency : bool) (tref : EType.t) (tsrc : EType.t) : unit =
  match (tref.it, tsrc.it) with
  | (TupleType tsref, TupleType tssrc) -> (
    try check_tuple_elements tsrc.at congruency tsref tssrc
    with Eslerr.Typing_error _ as exn ->
      Eslerr.(push_tp (terr_msg congruency tref tsrc) exn) |> raise )
  | _ -> Eslerr.(internal __FUNCTION__ (Expecting "Tuple type"))

and check_tuple_elements (at : Source.region) (congruency : bool)
  (tsref : EType.t list) (tssrc : EType.t list) : unit =
  let nesl_src nref nsrc =
    Eslerr.ErrSrc.region
      (if nsrc > nref then (List.nth tssrc (nsrc - nref)).at else at)
  in
  let check_element_f i (tref', tsrc') =
    type_check_werr ~congruency tref' tsrc' (IncompatibleElement (i + 1))
  in
  try List.combine tsref tssrc |> List.iteri check_element_f
  with Invalid_argument _ ->
    let (nref, nsrc) = (List.length tsref, List.length tssrc) in
    Eslerr.(typing ~src:(nesl_src nref nsrc) (NExpectedElements (nref, nsrc)))

and check_union_congruency (tref : EType.t) (tsrc : EType.t) : unit =
  let err_src (isref, ttar) t = Eslerr.ErrSrc.at (if isref then t else ttar) in
  let has_any ts = List.exists (fun t -> Source.(t.it == AnyType)) ts in
  let check_congruency_f (isref, ttar) ts t =
    try ignore (List.find (is_typeable ~congruency:true t) ts)
    with Not_found ->
      Eslerr.(typing ~src:(err_src (isref, ttar) t) (BadCongruency (ttar, t)))
  in
  let is_congruent tsref tssrc =
    if not (has_any tsref || has_any tssrc) then (
      List.iter (check_congruency_f (true, tref) tsref) tssrc;
      List.iter (check_congruency_f (false, tsrc) tssrc) tsref )
  in
  match (tref.it, tsrc.it) with
  | (UnionType tsref, UnionType tssrc) -> (
    try is_congruent tsref tssrc
    with Eslerr.Typing_error _ as exn ->
      Eslerr.(push_tp (BadCongruency (tref, tsrc)) exn |> raise) )
  | _ -> Eslerr.(internal __FUNCTION__ (Expecting "Union type"))

and check_union_subtyping_src (tref : EType.t) (tsrc : EType.t) : unit =
  let open Source in
  let has_any ts = List.exists (fun t -> Source.(t.it == AnyType)) ts in
  let check_src_types tssrc = List.iter (type_check tref) tssrc in
  match (tref.it, tsrc.it) with
  | (_, UnionType tssrc) when has_any tssrc -> ()
  | (_, UnionType tssrc) -> (
    try check_src_types tssrc
    with Eslerr.Typing_error _ as exn ->
      Eslerr.(push_tp (BadSubtyping (tref, tsrc)) exn |> raise) )
  | _ -> Eslerr.(internal __FUNCTION__ (Expecting "Union type source"))

and check_union_subtyping_ref (tref : EType.t) (tsrc : EType.t) : unit =
  let is_typeable_f tsrc tref = is_typeable ~congruency:false tref tsrc in
  match tref.it with
  | UnionType tsref -> (
    try ignore (List.find (is_typeable_f tsrc) tsref)
    with Not_found ->
      Eslerr.(typing ~src:(ErrSrc.at tsrc)) (BadSubtyping (tref, tsrc)) )
  | _ -> failwith "T_Subtyping.check_union_subtyping_ref"
