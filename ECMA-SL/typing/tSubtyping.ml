open EslCore
open EType

let terr_msg (congruency : bool) (tref : EType.t) (tsrc : EType.t) :
  Typing_error.msg =
  if congruency then BadCongruency (tref, tsrc) else BadSubtyping (tref, tsrc)

let resolve_optfld (t : t) : t =
  let open Source in
  let t_undefined = UndefinedType @> no_region in
  match t.it with
  | UnionType ts when List.exists (fun t' -> t'.it = UndefinedType) ts -> t
  | UnionType ts -> UnionType (List.append ts [ t_undefined ]) @> t.at
  | UndefinedType -> t
  | _ -> UnionType [ t; t_undefined ] @> t.at

let resolve_sigma_case_discriminant (at : Source.region) (dsc : Id.t)
  (tobj : tobject) : t =
  let open Typing_error in
  match Hashtbl.find_opt tobj.flds dsc.it with
  | Some (_, ft, _) -> ft
  | None -> throw ~src:(ErrSrc.region at) (MissingSigmaCaseDiscriminant dsc)

let unfold_sigma_case (dsc : Id.t) (t : t) : t * t =
  match t.it with
  | ObjectType tobj -> (
    try (resolve_sigma_case_discriminant t.at dsc tobj, t)
    with Typing_error.Error _ ->
      Internal_error.(throw __FUNCTION__ (Expecting "discriminant field")) )
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "object type"))

let rec type_check ?(congruency : bool = false) (tref : t) (tsrc : t) : unit =
  match (congruency, tref.it, tsrc.it) with
  | (_, _, _) when EType.equal tref tsrc -> ()
  | (_, _, AnyType) -> ()
  | (_, AnyType, _) -> ()
  | (_, ObjectType _, ObjectType _) -> check_object congruency tref tsrc
  | (_, ListType _, ListType _) -> check_list congruency tref tsrc
  | (_, TupleType _, TupleType _) -> check_tuple congruency tref tsrc
  | (_, SigmaType _, SigmaType _) -> check_sigma congruency tref tsrc
  | (true, UnionType _, UnionType _) -> check_union_congruency tref tsrc
  | (false, _, UnionType _) -> check_union_subtyping_src tref tsrc
  | (false, UnionType _, _) -> check_union_subtyping_ref tref tsrc
  | (false, SigmaType _, ObjectType _) -> check_sigma_folding tref tsrc
  | (false, UnknownType, _) -> ()
  | (false, _, NeverType) -> ()
  | (false, IntType, LiteralType (IntegerLit _)) -> ()
  | (false, FloatType, LiteralType (FloatLit _)) -> ()
  | (false, StringType, LiteralType (StringLit _)) -> ()
  | (false, BooleanType, LiteralType (BooleanLit _)) -> ()
  | (false, SymbolType, LiteralType (SymbolLit _)) -> ()
  | (_, _, _) ->
    Typing_error.(throw ~src:(ErrSrc.at tsrc) (terr_msg congruency tref tsrc))

and is_typeable ?(congruency : bool = false) (tref : t) (tsrc : t) : bool =
  try type_check ~congruency tref tsrc |> fun () -> true
  with Typing_error.Error _ -> false

and type_check_werr ?(congruency : bool = false) (tref : t) (tsrc : t)
  (msg : Typing_error.msg) : unit =
  try type_check ~congruency tref tsrc
  with Typing_error.Error _ as exn -> Typing_error.(push msg exn |> raise)

and check_object (congruency : bool) (tref : t) (tsrc : t) : unit =
  let check_object_type otref otsrc =
    Hashtbl.iter (check_object_fields congruency otsrc.kind otref) otsrc.flds;
    Hashtbl.iter (check_missing_fields tsrc.at congruency otsrc) otref.flds;
    check_summary_type tsrc.at congruency otref otsrc
  in
  match (tref.it, tsrc.it) with
  | (ObjectType tobjref, ObjectType tobjsrc) -> (
    try check_object_type tobjref tobjsrc
    with Typing_error.Error _ as exn ->
      Typing_error.(push (terr_msg congruency tref tsrc) exn |> raise) )
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "object type"))

and check_object_fields (congruency : bool) (tobjkind : EType.tobjkind)
  (tobjref : EType.tobject) (_ : Id.t')
  ((sfn, sft, sfs) : Id.t * EType.t * EType.tfldstyle) : unit =
  let is_obj = function ObjectType _ -> true | _ -> false in
  let fld_congruency rft = Source.(tobjkind == ObjSto && not (is_obj rft.it)) in
  let fld_err msg = Typing_error.(throw ~src:(ErrSrc.at sfn) msg) in
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
  if congruency || tobjsrc.kind == ObjSto || rfs == FldReq then
    if not (Hashtbl.mem tobjsrc.flds rfn.it) then
      Typing_error.(throw ~src:(ErrSrc.region at) (MissingField rfn))

and check_summary_type (at : Source.region) (congruency : bool)
  (tobjref : EType.tobject) (tobjsrc : EType.tobject) : unit =
  let smry_err msg = Typing_error.(throw ~src:(ErrSrc.region at) msg) in
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
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "list type"))

and check_tuple (congruency : bool) (tref : EType.t) (tsrc : EType.t) : unit =
  match (tref.it, tsrc.it) with
  | (TupleType tsref, TupleType tssrc) -> (
    try check_tuple_elements tsrc.at congruency tsref tssrc
    with Typing_error.Error _ as exn ->
      Typing_error.(push (terr_msg congruency tref tsrc) exn) |> raise )
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "tuple type"))

and check_tuple_elements (at : Source.region) (congruency : bool)
  (tsref : EType.t list) (tssrc : EType.t list) : unit =
  let open Typing_error in
  let check_element_f i (tref', tsrc') =
    type_check_werr ~congruency tref' tsrc' (IncompatibleElement (i + 1))
  in
  try List.combine tsref tssrc |> List.iteri check_element_f
  with Invalid_argument _ ->
    let (nref, nsrc) = (List.length tsref, List.length tssrc) in
    let at = if nsrc > nref then (List.nth tssrc (nsrc - nref)).at else at in
    throw ~src:(ErrSrc.region at) (NExpectedElements (nref, nsrc))

and check_union_congruency (tref : EType.t) (tsrc : EType.t) : unit =
  let open Typing_error in
  let err_src (isref, ttar) t = ErrSrc.at (if isref then t else ttar) in
  let has_any ts = List.exists (fun t -> Source.(t.it == AnyType)) ts in
  let check_congruency_f (isref, ttar) ts t =
    try ignore (List.find (is_typeable ~congruency:true t) ts)
    with Not_found ->
      throw ~src:(err_src (isref, ttar) t) (BadCongruency (ttar, t))
  in
  let is_congruent tsref tssrc =
    if not (has_any tsref || has_any tssrc) then (
      List.iter (check_congruency_f (true, tref) tsref) tssrc;
      List.iter (check_congruency_f (false, tsrc) tssrc) tsref )
  in
  match (tref.it, tsrc.it) with
  | (UnionType tsref, UnionType tssrc) -> (
    try is_congruent tsref tssrc
    with Error _ as exn -> push (BadCongruency (tref, tsrc)) exn |> raise )
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "union type"))

and check_union_subtyping_src (tref : EType.t) (tsrc : EType.t) : unit =
  let open Source in
  let has_any ts = List.exists (fun t -> Source.(t.it == AnyType)) ts in
  let check_src_types tssrc = List.iter (type_check tref) tssrc in
  match (tref.it, tsrc.it) with
  | (_, UnionType tssrc) when has_any tssrc -> ()
  | (_, UnionType tssrc) -> (
    try check_src_types tssrc
    with Typing_error.Error _ as exn ->
      Typing_error.(push (BadSubtyping (tref, tsrc)) exn |> raise) )
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "union type source"))

and check_union_subtyping_ref (tref : EType.t) (tsrc : EType.t) : unit =
  let is_typeable_f tsrc tref = is_typeable ~congruency:false tref tsrc in
  match tref.it with
  | UnionType tsref -> (
    try ignore (List.find (is_typeable_f tsrc) tsref)
    with Not_found ->
      Typing_error.(throw ~src:(ErrSrc.at tsrc)) (BadSubtyping (tref, tsrc)) )
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "union type ref"))

and check_sigma (congruency : bool) (tref : t) (tsrc : t) : unit =
  let check_sigma_type (dscref, cssref) (dscsrc, csssrc) =
    check_sigma_discriminant dscref dscsrc;
    let cssref' = List.map (unfold_sigma_case dscref) cssref in
    let csssrc' = List.map (unfold_sigma_case dscsrc) csssrc in
    let sigcss = combine_sigma_cases tsrc.at congruency cssref' csssrc' in
    List.iter (check_sigma_case congruency) sigcss
  in
  match (tref.it, tsrc.it) with
  | (SigmaType (dscref, cssref), SigmaType (dscsrc, csssrc)) -> (
    try check_sigma_type (dscref, cssref) (dscsrc, csssrc)
    with Typing_error.Error _ as exn ->
      Typing_error.(push (terr_msg congruency tref tsrc) exn |> raise) )
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "sigma type"))

and check_sigma_discriminant (dscref : Id.t) (dscsrc : Id.t) =
  if not (String.equal dscref.it dscsrc.it) then
    Typing_error.(throw ~src:(ErrSrc.at dscsrc) IncompatibleSigmaDiscriminant)

and combine_sigma_cases (at : Source.region) (congruency : bool)
  (cssref : (t * t) list) (csssrc : (t * t) list) : (t * t * t) list =
  let match_cs_f tdsc1 (tdsc2, _) = EType.equal tdsc1 tdsc2 in
  let find_cs (tdscsrc, tcssrc) =
    try snd (List.find (match_cs_f tdscsrc) cssref)
    with Not_found ->
      Typing_error.(throw ~src:(ErrSrc.at tcssrc) (ExtraSigmaCase tdscsrc))
  in
  let check_missing_css (tdscref, _) =
    if not (List.exists (match_cs_f tdscref) csssrc) then
      Typing_error.(throw ~src:(ErrSrc.region at) (MissingSigmaCase tdscref))
  in
  let combine_cs_f (tdsc, tcssrc) = (tdsc, find_cs (tdsc, tcssrc), tcssrc) in
  let sigcss = List.map combine_cs_f csssrc in
  if congruency then List.iter check_missing_css cssref;
  sigcss

and check_sigma_case (congruency : bool) ((dsc, csref, cssrc) : t * t * t) :
  unit =
  try type_check ~congruency csref cssrc
  with Typing_error.Error _ as exn ->
    Typing_error.(push (IncompatibleSigmaCase dsc) exn |> raise)

and check_sigma_folding (tref : t) (tsrc : t) : unit =
  let check_sigma_case (dscref, cssref) tobjsrc =
    let cssref' = List.map (unfold_sigma_case dscref) cssref in
    let tdscsrc = resolve_sigma_case_discriminant tsrc.at dscref tobjsrc in
    let csref = match_sigma_case cssref' (tdscsrc, tsrc) in
    check_sigma_case false (tdscsrc, csref, tsrc)
  in
  match (tref.it, tsrc.it) with
  | (SigmaType (dscref, cssref), ObjectType tobjsrc) -> (
    try check_sigma_case (dscref, cssref) tobjsrc
    with Typing_error.Error _ as exn ->
      Typing_error.(push (terr_msg false tref tsrc) exn |> raise) )
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "sigma|object type"))

and match_sigma_case (cssref : (t * t) list) ((tdscsrc, tsrc) : t * t) : t =
  let open Typing_error in
  let match_cs_f tdsc1 (tdsc2, _) = EType.equal tdsc1 tdsc2 in
  try snd (List.find (match_cs_f tdscsrc) cssref)
  with Not_found ->
    throw ~src:(ErrSrc.at tsrc) (UnknownSigmaCaseDiscriminant tdscsrc)
