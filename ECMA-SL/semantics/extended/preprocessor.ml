module Imports = struct
  let rec resolve_imports (load_dependency_fun : Id.t -> EProg.t) (p : EProg.t)
    (resolved : (string, unit) Hashtbl.t) (paths : string list)
    (unresolved : Id.t list list) : unit =
    let resolve_imports' = resolve_imports load_dependency_fun p resolved in
    let open EParsing_helper.EProg in
    match (unresolved, paths) with
    | ([], _) -> ()
    | ([] :: unresolved', path :: paths) ->
      Hashtbl.replace resolved path ();
      resolve_imports' paths unresolved'
    | ((file :: files') :: _, _) ->
      if Hashtbl.mem resolved file.it then resolve_imports' paths [ files' ]
      else if not (List.mem file.it paths) then (
        let dependency = load_dependency_fun file in
        let unresolved' = EProg.imports dependency :: unresolved in
        Hashtbl.iter (fun _ t -> parse_tdef t p) (EProg.tdefs dependency);
        Hashtbl.iter (fun _ f -> parse_func f p) (EProg.funcs dependency);
        Hashtbl.iter (fun _ m -> parse_macro m p) (EProg.macros dependency);
        resolve_imports' (file.it :: paths) unresolved' )
      else Eslerr.(compile ~src:(ErrSrc.at file) (CyclicDependency file))
    | ([] :: _, []) ->
      Eslerr.(internal __FUNCTION__ (Expecting "non-empty path"))
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

  let apply_macros (p : EProg.t) : unit =
    Hashtbl.iter (fun _ f -> apply_func_macros p f) p.funcs
end
