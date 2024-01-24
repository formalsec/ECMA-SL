open Source
open EPat

type sigmaCase_t = (string, EType.t option) Hashtbl.t
type sigmaObject_t = EType.t * sigmaCase_t list
type sigmaModel_t = (EType.t, sigmaObject_t) Hashtbl.t
type patValFld_t = string * EPat.pv * EType.t option
type patVarFld_t = string * string
type patUpdates_t = (string * EType.t) list

type patResult_t =
  | Succ of patUpdates_t
  | Err of T_Err.t * string list

let generate_sigma_cases (flds : (string, EType.tfld_t) Hashtbl.t) :
  sigmaCase_t list =
  let _unfold_ft_f ft =
    let ts = EType.unfold_type true (fst ft) in
    let ftUnf = List.map (fun t -> Some t) ts in
    if EType.tfld_is_opt ft then None :: ftUnf else ftUnf
  in
  let _cartesian_prod_f r (fn, fts) =
    List.map (fun r' -> List.map (fun ft -> Seq.cons (fn, ft) r') fts) r
    |> List.concat
  in
  Hashtbl.to_seq (Hashtbl.copy flds)
  |> Seq.map (fun (fn, ft) -> (fn, _unfold_ft_f ft))
  |> Seq.fold_left _cartesian_prod_f [ List.to_seq [] ]
  |> List.map Hashtbl.of_seq

let generate_sigma_model (ts : EType.t list) (d : string) : sigmaModel_t =
  let _generate_sigma_object_f t =
    match t with
    | EType.ObjectType tobj ->
      let dt = EType.tfld_t (EType.find_tfld tobj d) in
      let sigmaCases = generate_sigma_cases (EType.flds tobj) in
      (dt, (t, sigmaCases))
    | _ -> failwith "Typed ECMA-SL: T_Pattern.generate_sigma_model"
  in
  Hashtbl.of_seq (List.to_seq (List.map _generate_sigma_object_f ts))

let get_pattern_vars (pat : EPat.t) : string list =
  let _get_obj_pat_vars patFlds =
    let _filter_f (_, pv) = match pv with PatVar s -> Some s | _ -> None in
    List.filter_map _filter_f patFlds
  in
  match pat.it with
  | ObjPat (patFlds, _) -> _get_obj_pat_vars patFlds
  | DefaultPat -> []

let parse_pattern_val (patVal : EPat.pv) : EPat.pv * EType.t =
  match patVal with
  | PatVal v -> (patVal, EType.parse_literal_type v)
  | _ -> failwith "Typed ECMA-SL: T_Pattern:parse_pat_val"

let test_duplicated_pattern_flds (patFlds : (Id.t * EPat.pv) list) : unit =
  let _check_duplicates_f r (pn, _) =
    if not (List.mem pn.it r) then pn.it :: r
    else
      T_Err.raise (T_Err.DuplicatedPatternFld pn.it) ~tkn:(T_Err.str_tkn pn.it)
  in
  ignore (List.fold_left _check_duplicates_f [] patFlds)

let get_pattern_fld (tobj : EType.t) (fn : string) : EType.tfld_t =
  match EType.find_tfld_opt (EType.get_tobj tobj) fn with
  | Some tfld -> tfld
  | None -> T_Err.raise (T_Err.BadLookup (fn, tobj)) ~tkn:(T_Err.str_tkn fn)

let get_pattern_discriminant (patFlds : (Id.t * EPat.pv) list) (d : string) :
  EPat.pv * EType.t =
  let _parse_pattern_value pv =
    try parse_pattern_val pv
    with _ -> T_Err.raise (T_Err.BadDiscriminant d) ~tkn:(T_Err.patval_tkn pv)
  in
  match List.find_opt (fun (fn, _) -> fn.it = d) patFlds with
  | Some (_, pv) -> _parse_pattern_value pv
  | None -> T_Err.raise (T_Err.MissingDiscriminant d)

let split_pattern_flds (patFlds : (Id.t * EPat.pv) list) :
  patValFld_t list * patVarFld_t list =
  let _get_pt pv = Some (snd (parse_pattern_val pv)) in
  let _split_pattern_f (pn, pv) (patValFlds, patVarFlds) =
    match pv with
    | PatVal _v -> ((pn.it, pv, _get_pt pv) :: patValFlds, patVarFlds)
    | PatVar x -> (patValFlds, (pn.it, x) :: patVarFlds)
    | PatNone -> ((pn.it, pv, None) :: patValFlds, patVarFlds)
  in
  List.fold_right _split_pattern_f patFlds ([], [])

let update_sigma_object ((tobj, sigmaCases) : sigmaObject_t)
  (patValFlds : patValFld_t list) : sigmaObject_t =
  let _test_pattern_val tfld _pn pv pt =
    let ft = EType.tfld_t tfld in
    if T_Typing.is_typeable ft pt then List.mem pt (EType.unfold_type false ft)
    else T_Err.raise (T_Err.BadValPattern (ft, pt)) ~tkn:(T_Err.patval_tkn pv)
  in
  let _test_pattern_vals_f (pn, pv, pt) =
    let tfld = get_pattern_fld tobj pn in
    match (EType.tfld_is_opt tfld, pt) with
    | (false, None) ->
      T_Err.raise T_Err.BadNonePattern ~tkn:(T_Err.patval_tkn pv)
    | (_, Some pt') -> _test_pattern_val tfld pn pv pt'
    | (_, None) -> _test_pattern_val tfld pn pv EType.UndefinedType
  in
  let _check_bad_case_f patValFdls sigmaCase =
    let _test_case_forms_f sigmaCase ((pn, _, pt), update) =
      if update then Hashtbl.find sigmaCase pn = pt else true
    in
    not (List.for_all (_test_case_forms_f sigmaCase) patValFdls)
  in
  let updateModelFlags = List.map _test_pattern_vals_f patValFlds in
  let patValFlds' = List.combine patValFlds updateModelFlags in
  let badSigmaCases = List.filter (_check_bad_case_f patValFlds') sigmaCases in
  let isUnusedSigmaCase = List.length badSigmaCases == List.length sigmaCases in
  let updatesModel = not (List.mem false updateModelFlags) in
  match (isUnusedSigmaCase, updatesModel) with
  | (false, true) -> (tobj, badSigmaCases)
  | (false, false) -> (tobj, sigmaCases)
  | (true, _) ->
    T_Err.raise T_Err.UnusedPatternCase ~kind:(T_Err.warning_kind ())

let generate_pattern_updates ((tobj, sigmaCases) : sigmaObject_t)
  (patVarFlds : patVarFld_t list) : patUpdates_t =
  let _generate_pattern_update_f (pn, x) =
    let _get_ft = function None -> EType.UndefinedType | Some ft' -> ft' in
    let _get_ts_f sigmaCase = _get_ft (Hashtbl.find sigmaCase x) in
    let _ = get_pattern_fld tobj pn in
    let ts = List.map _get_ts_f sigmaCases in
    (x, T_Narrowing.narrow_type (EType.UnionType ts))
  in
  List.map _generate_pattern_update_f patVarFlds

let type_obj_pattern (sigmaModel : sigmaModel_t) (d : string)
  (patFlds : (Id.t * EPat.pv) list) : patUpdates_t =
  let _ = test_duplicated_pattern_flds patFlds in
  let (dv, dt) = get_pattern_discriminant patFlds d in
  let (patValFlds, patVarFlds) = split_pattern_flds patFlds in
  match Hashtbl.find_opt sigmaModel dt with
  | Some sigmaObject ->
    let sigmaObject' = update_sigma_object sigmaObject patValFlds in
    Hashtbl.replace sigmaModel dt sigmaObject' |> fun () ->
    generate_pattern_updates sigmaObject patVarFlds
  | None ->
    T_Err.raise (T_Err.UnknownDiscriminant dt) ~tkn:(T_Err.patval_tkn dv)

let type_default_pattern (sigmaModel : sigmaModel_t) : patUpdates_t =
  let _count_missing_cases_f d (tobj, sigmaCases) count =
    Hashtbl.replace sigmaModel d (tobj, []) |> fun () ->
    count + List.length sigmaCases
  in
  let nMissingCases = Hashtbl.fold _count_missing_cases_f sigmaModel 0 in
  if nMissingCases > 0 then []
  else T_Err.raise T_Err.UnusedPatternCase ~kind:(T_Err.warning_kind ())

let type_match_pattern (sigmaModel : sigmaModel_t) (d : string) (pat : EPat.t) :
  patResult_t =
  let _type_match_pattern () =
    match pat.it with
    | ObjPat (patFlds, _) -> type_obj_pattern sigmaModel d patFlds
    | DefaultPat -> type_default_pattern sigmaModel
  in
  try Succ (_type_match_pattern ())
  with T_Err.TypeError terr ->
    Err ({ terr with T_Err.src = T_Err.pat_tkn pat }, get_pattern_vars pat)

let test_complete_model (sigmaModel : sigmaModel_t) : unit =
  let _get_ft = function None -> EType.UndefinedType | Some ft' -> ft' in
  let _get_tfld ft = (_get_ft ft, EType.Required) in
  let _construct_case_tobj sigmaCase =
    Seq.map (fun (fn, ft) -> (fn, _get_tfld ft)) (Hashtbl.to_seq sigmaCase)
    |> fun flds -> EType.create_tobj (Hashtbl.of_seq flds) None
  in
  let _test_empty_cases_f _ (_, sigmaCases) =
    if List.length sigmaCases > 0 then
      let sigmaCase = List.nth sigmaCases 0 in
      let tobj = _construct_case_tobj sigmaCase in
      T_Err.raise T_Err.MissingPatternCase ~kind:(T_Err.warning_kind ())
        ~src:(T_Err.type_tkn tobj)
  in
  Hashtbl.iter _test_empty_cases_f sigmaModel
