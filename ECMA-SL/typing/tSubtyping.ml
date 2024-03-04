open EType

let terr_msg (congruency : bool) (tref : EType.t) (tsrc : EType.t) :
  Eslerr.tperr =
  if congruency then BadCongruency (tref, tsrc) else BadSubtyping (tref, tsrc)

let terr (congruency : bool) (tref : EType.t) (tsrc : EType.t) : 'a =
  let msg = terr_msg congruency tref tsrc in
  Eslerr.(typing ~src:(ErrSrc.at tsrc) msg)

let rec type_check ?(congruency : bool = false) (tref : t) (tsrc : t) : unit =
  match (congruency, tref.it, tsrc.it) with
  | (_, _, _) when EType.equal tref tsrc -> ()
  | (_, _, AnyType) -> ()
  | (_, AnyType, _) -> ()
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
  | (_, _, _) -> terr congruency tref tsrc

and is_typeable ?(congruency : bool = false) (tref : t) (tsrc : t) : bool =
  try type_check ~congruency tref tsrc |> fun () -> true
  with Eslerr.Typing_error _ -> false

and check_list (congruency : bool) (tref : EType.t) (tsrc : EType.t) : unit =
  match (tref.it, tsrc.it) with
  | (ListType tref', ListType tsrc') -> (
    try type_check ~congruency tref' tsrc'
    with Eslerr.Typing_error _ as exn ->
      Eslerr.(push_tp (terr_msg congruency tref tsrc) exn) |> raise )
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
    let open Eslerr in
    if nsrc > nref then ErrSrc.at (List.nth tssrc (nsrc - nref))
    else ErrSrc.region at
  in
  let get_elements () =
    try List.combine tsref tssrc
    with Invalid_argument _ ->
      let (nref, nsrc) = (List.length tsref, List.length tssrc) in
      Eslerr.(typing ~src:(nesl_src nref nsrc) (NExpectedElements (nref, nsrc)))
  in
  let check_element_f i (tref', tsrc') =
    try type_check ~congruency tref' tsrc'
    with Eslerr.Typing_error _ as exn ->
      Eslerr.(push_tp (IncompatibleElement (i + 1)) exn |> raise)
  in
  List.iteri check_element_f (get_elements ())

and check_union_congruency (tref : EType.t) (tsrc : EType.t) : unit =
  let err_src (isref, ttar) t = Eslerr.ErrSrc.at (if isref then t else ttar) in
  let has_any ts = List.exists (fun t -> Source.(t.it = AnyType)) ts in
  let check_congruency_f (isref, ttar) ts t =
    try ignore (List.find (is_typeable ~congruency:true t) ts)
    with Not_found ->
      Eslerr.(typing ~src:(err_src (isref, ttar) t) (BadCongruency (ttar, t)))
  in
  let is_congruent tsref tssrc =
    if has_any tsref || has_any tssrc then ()
    else (
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
  let has_any ts = List.exists (fun t -> Source.(t.it = AnyType)) ts in
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
