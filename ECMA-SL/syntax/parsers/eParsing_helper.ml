open Source
open Compile_error

module Expr = struct
  open EExpr

  let parse_object_fields (flds : (Id.t * t) list) : (Id.t * t) list =
    let check_dups checked (fn, _) =
      if not (Hashtbl.mem checked fn.it) then Hashtbl.replace checked fn.it ()
      else throw ~src:(ErrSrc.at fn) (DuplicatedField fn)
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
      else throw ~src:(ErrSrc.at fn) (DuplicatedTField fn)
    in
    let retrieve_smry_field tflds =
      let make_smry (fn, ft, _) = (fn, ft) in
      let smry = Option.map make_smry (Hashtbl.find_opt tflds "*") in
      Hashtbl.remove tflds "*";
      smry
    in
    let tflds = Hashtbl.create (List.length flds) in
    List.iter (parse_tobjfld_f tflds) flds;
    let smry = retrieve_smry_field tflds in
    { kind = ObjLit; flds = tflds; smry }

  let parse_tsigma (dsc : Id.t) (t : t) : t list =
    let parse_dsc checked ot at =
      match Hashtbl.find_opt ot.flds dsc.it with
      | Some (_, ({ it = LiteralType lt; _ } as tdsc), _) ->
        if not (Hashtbl.mem checked lt) then Hashtbl.replace checked lt ()
        else throw ~src:(ErrSrc.at tdsc) (DuplicatedSigmaDiscriminant tdsc)
      | Some (_, t', _) -> throw ~src:(ErrSrc.at t') UnexpectedSigmaDiscriminant
      | None -> throw ~src:(ErrSrc.region at) (MissingSigmaDiscriminant dsc)
    in
    let parse_case_f checked = function
      | { it = ObjectType ot; at } -> parse_dsc checked ot at
      | t' -> throw ~src:(ErrSrc.at t') UnexpectedSigmaCase
    in
    let sigma_cases = function
      | { it = UnionType ts; _ } -> ts
      | { it = ObjectType _; _ } as t -> [ t ]
      | t' -> throw ~src:(ErrSrc.at t') UnexpectedSigmaCase
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
    | Some _ -> throw ~src:(ErrSrc.at tn) (DuplicatedTDef tn)

  let parse_func (f : EFunc.t) (p : t) : unit =
    let fn = EFunc.name f in
    match Hashtbl.find_opt p.funcs fn.it with
    | None -> Hashtbl.replace p.funcs fn.it f
    | Some _ -> throw ~src:(ErrSrc.at fn) (DuplicatedFunc fn)

  let parse_macro (m : EMacro.t) (p : t) : unit =
    let mn = EMacro.name m in
    match Hashtbl.find_opt p.macros mn.it with
    | None -> Hashtbl.replace p.macros mn.it m
    | Some _ -> throw ~src:(ErrSrc.at mn) (DuplicatedMacro mn)

  let parse_prog (imports : Id.t list) (el_parsers : (t -> unit) list) : t =
    let p = { (default ()) with imports } in
    List.iter (fun el_parser -> el_parser p) el_parsers;
    p
end
