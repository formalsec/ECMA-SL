open Source

module EExpr = struct
  open EExpr

  let parse_object_fields (flds : (Id.t * t) list) : (Id.t * t) list =
    let check_dups checked (fn, _) =
      if not (Hashtbl.mem checked fn.it) then Hashtbl.replace checked fn.it ()
      else Eslerr.(compile ~src:(ErrSrc.at fn) (DuplicatedField fn.it))
    in
    List.iter (check_dups (Hashtbl.create (List.length flds))) flds;
    flds
end
