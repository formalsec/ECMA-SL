open EslBase
open EslSyntax

module Imports = struct
  let load_dependency (file : Id.t) (path : string) : EProg.t =
    try EParsing.(load_file ~file:file.it path |> parse_eprog ~file:file.it path)
    with Not_found ->
      Compile_error.(throw ~src:(ErrSrc.at file) (UnknownDependency file))

  let relativize (file : Id.t') (imports : Id.t list) : Id.t list =
    let open Source in
    let relativize_f dir import = Filename.concat dir import.it @> import.at in
    List.map (relativize_f (Filename.dirname file)) imports

  let rec import_resolver (workspace : string) (p : EProg.t)
    (resolved : (Id.t', unit) Hashtbl.t) (unresolved : (Id.t' * Id.t list) list)
    : unit =
    match unresolved with
    | [] -> ()
    | (source, []) :: unresolved' ->
      Hashtbl.replace resolved source ();
      import_resolver workspace p resolved unresolved'
    | (source, import :: imports') :: unresolved' ->
      if Hashtbl.mem resolved import.it then
        import_resolver workspace p resolved ((source, imports') :: unresolved')
      else if List.exists (fun (path, _) -> path = import.it) unresolved then
        Compile_error.(throw ~src:(ErrSrc.at import) (CyclicDependency import))
      else
        let open EParsing_helper.Prog in
        let dependency_path = Filename.concat workspace import.it in
        let dependency = load_dependency import dependency_path in
        let dependency_imports = relativize source (EProg.imports dependency) in
        let new_dependencies = (import.it, dependency_imports) in
        Hashtbl.iter (fun _ t -> parse_tdef t p) (EProg.tdefs dependency);
        Hashtbl.iter (fun _ f -> parse_func f p) (EProg.funcs dependency);
        Hashtbl.iter (fun _ m -> parse_macro m p) (EProg.macros dependency);
        import_resolver workspace p resolved (new_dependencies :: unresolved)

  let resolve_imports (p : EProg.t) : EProg.t =
    let workspace = Filename.dirname (EProg.path p) in
    let resolved = Hashtbl.create !Base.default_hashtbl_sz in
    let relative_imports = relativize (EProg.file p) (EProg.imports p) in
    import_resolver workspace p resolved [ (EProg.file p, relative_imports) ];
    { p with imports = [] }
end

module Macros = struct
  let macro_mapper (p : EProg.t) : EStmt.t -> EStmt.t =
   fun s ->
    match s.it with
    | EStmt.MacroApply (mn, es) -> (
      match Hashtbl.find_opt (EProg.macros p) mn.it with
      | None -> Compile_error.(throw ~src:(ErrSrc.at mn) (UnknownMacro mn))
      | Some m ->
        let pxs = EMacro.params' m in
        let subst =
          try List.combine pxs es |> List.to_seq |> Hashtbl.of_seq
          with _ ->
            let (npxs, nargs) = (List.length pxs, List.length es) in
            Compile_error.(throw ~src:(ErrSrc.at s) (BadNArgs (npxs, nargs)))
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
    { p with macros = Hashtbl.create !Base.default_hashtbl_sz }
end
