module Imports = struct
  let load_dependency (file : Id.t) : EProg.t =
    let open Parsing_utils in
    try load_file file.it |> parse_eprog ~file:file.it
    with _ -> Eslerr.(compile ~src:(ErrSrc.at file) (UnknownDependency file))

  let rec import_resolver (p : EProg.t) (resolved : (string, unit) Hashtbl.t)
    (paths : string list) (unresolved : Id.t list list) : unit =
    let open EParsing_helper.EProg in
    match (unresolved, paths) with
    | ([], _) -> ()
    | ([] :: unresolved', path :: paths) ->
      Hashtbl.replace resolved path ();
      import_resolver p resolved paths unresolved'
    | ((file :: files') :: _, _) ->
      if Hashtbl.mem resolved file.it then
        import_resolver p resolved paths [ files' ]
      else if not (List.mem file.it paths) then (
        let dependency = load_dependency file in
        let unresolved' = EProg.imports dependency :: unresolved in
        Hashtbl.iter (fun _ t -> parse_tdef t p) (EProg.tdefs dependency);
        Hashtbl.iter (fun _ f -> parse_func f p) (EProg.funcs dependency);
        Hashtbl.iter (fun _ m -> parse_macro m p) (EProg.macros dependency);
        import_resolver p resolved (file.it :: paths) unresolved' )
      else Eslerr.(compile ~src:(ErrSrc.at file) (CyclicDependency file))
    | ([] :: _, []) ->
      Eslerr.(internal __FUNCTION__ (Expecting "non-empty path"))

  let resolve_imports (p : EProg.t) : EProg.t =
    let resolved = Hashtbl.create !Config.default_hashtbl_sz in
    import_resolver p resolved [ EProg.file p ] [ EProg.imports p ];
    { p with imports = [] }
end

module Macros = struct
  let macro_mapper (p : EProg.t) : EStmt.t -> EStmt.t =
   fun s ->
    match s.it with
    | EStmt.MacroApply (mn, es) -> (
      match Hashtbl.find_opt (EProg.macros p) mn.it with
      | None -> Eslerr.(compile ~src:(ErrSrc.at mn) (UnknownMacro mn))
      | Some m ->
        let pxs = EMacro.params' m in
        let subst =
          try List.combine pxs es |> List.to_seq |> Hashtbl.of_seq
          with _ ->
            let (npxs, nargs) = (List.length pxs, List.length es) in
            Eslerr.(compile ~src:(ErrSrc.at s) (BadNArgs (npxs, nargs)))
        in
        EStmt.map ~emapper:(EExpr.Mapper.var subst) EStmt.Mapper.id
          (EMacro.body m) )
    | _ -> s

  let apply_func_macros (p : EProg.t) (f : EFunc.t) : unit =
    let body = EStmt.map (macro_mapper p) f.it.body in
    Hashtbl.replace (EProg.funcs p) (EFunc.name' f)
      { f with it = { f.it with body } }

  let apply_macros (p : EProg.t) : EProg.t =
    Hashtbl.iter (fun _ f -> apply_func_macros p f) p.funcs;
    { p with macros = Hashtbl.create !Config.default_hashtbl_sz }
end
