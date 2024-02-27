open Source

module EExpr = struct
  open EExpr

  let parse_object_fields (flds : (Id.t * t) list) : (Id.t * t) list =
    let check_dups checked (fn, _) =
      if not (Hashtbl.mem checked fn.it) then Hashtbl.replace checked fn.it ()
      else Eslerr.(compile ~src:(ErrSrc.at fn) (DuplicatedField fn))
    in
    List.iter (check_dups (Hashtbl.create (List.length flds))) flds;
    flds
end

module EProg = struct
  open EProg

  let parse_tdef (t : EType.tdef) (p : t) : unit =
    let tn = EType.tdef_name t in
    match Hashtbl.find_opt p.tdefs tn.it with
    | None -> Hashtbl.replace p.tdefs tn.it t
    | Some _ -> Eslerr.(compile ~src:(ErrSrc.at tn) (DuplicatedTdef tn))

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
