open Source

module Expr = struct
  open EExpr

  let parse_object_fields (flds : (Id.t * t) list) : (Id.t * t) list =
    let check_dups checked (fn, _) =
      if not (Hashtbl.mem checked fn.it) then Hashtbl.replace checked fn.it ()
      else Eslerr.(compile ~src:(ErrSrc.at fn) (DuplicatedField fn))
    in
    List.iter (check_dups (Hashtbl.create (List.length flds))) flds;
    flds
end

module Type = struct
  open EType

  let parse_tobject (flds : (Id.t * t * tfldstyle) list) : tobject =
    let parse_tobjfld_f tflds (fn, ft, fs) =
      if not (Hashtbl.mem tflds fn.it) then
        Hashtbl.replace tflds fn.it (fn, ft, fs)
      else Eslerr.(compile ~src:(ErrSrc.at fn) (DuplicatedTField fn))
    in
    let tflds = Hashtbl.create (List.length flds) in
    List.iter (parse_tobjfld_f tflds) flds;
    { kind = ObjLit; flds = tflds }

  let parse_tsigma (dsc : Id.t) (t : t) : t list =
    let parse_dsc dsc_checked ot at =
      match Hashtbl.find_opt ot.flds dsc.it with
      | Some (_, { it = LiteralType lt; _ }, _)
        when not (Hashtbl.mem dsc_checked lt) ->
        Hashtbl.replace dsc_checked lt ()
      | Some (_, ({ it = LiteralType _; _ } as lt), _) ->
        Eslerr.(compile ~src:(ErrSrc.at lt) (DuplicatedSigmaDiscriminant lt))
      | Some (_, t', _) ->
        Eslerr.(compile ~src:(ErrSrc.at t') UnexpectedSigmaDiscriminant)
      | None ->
        Eslerr.(compile ~src:(ErrSrc.region at) (MissingSigmaDiscriminant dsc))
    in
    let parse_case_f dsc_checked = function
      | { it = ObjectType ot; at } -> parse_dsc dsc_checked ot at
      | t' -> Eslerr.(compile ~src:(ErrSrc.at t') UnexpectedSigmaCase)
    in
    let sigma_cases = function
      | { it = UnionType ts; _ } -> ts
      | { it = ObjectType _; _ } as t -> [ t ]
      | t' -> Eslerr.(compile ~src:(ErrSrc.at t') UnexpectedSigmaCase)
    in
    let ts = sigma_cases t in
    List.iter (parse_case_f (Hashtbl.create (List.length ts))) ts;
    ts
end

module Prog = struct
  open EProg

  let parse_tdef (t : EType.TDef.t) (p : t) : unit =
    let tn = EType.TDef.name t in
    match Hashtbl.find_opt p.tdefs tn.it with
    | None -> Hashtbl.replace p.tdefs tn.it t
    | Some _ -> Eslerr.(compile ~src:(ErrSrc.at tn) (DuplicatedTDef tn))

  let parse_func (f : EFunc.t) (p : t) : unit =
    let fn = EFunc.name f in
    match Hashtbl.find_opt p.funcs fn.it with
    | None -> Hashtbl.replace p.funcs fn.it f
    | Some _ -> Eslerr.(compile ~src:(ErrSrc.at fn) (DuplicatedFunc fn))

  let parse_macro (m : EMacro.t) (p : t) : unit =
    let mn = EMacro.name m in
    match Hashtbl.find_opt p.macros mn.it with
    | None -> Hashtbl.replace p.macros mn.it m
    | Some _ -> Eslerr.(compile ~src:(ErrSrc.at mn) (DuplicatedMacro mn))

  let parse_prog (imports : Id.t list) (el_parsers : (t -> unit) list) : t =
    let p = { (default ()) with imports } in
    List.iter (fun el_parser -> el_parser p) el_parsers;
    p
end
