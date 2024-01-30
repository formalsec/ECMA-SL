type t =
  { file : string
  ; imports : Id.t list
  ; tdefs : (Id.t', EType.tdef) Hashtbl.t
  ; funcs : (Id.t', EFunc.t) Hashtbl.t
  ; macros : (Id.t', EMacro.t) Hashtbl.t
  }

let default () : t =
  { file = ""
  ; imports = []
  ; tdefs = Hashtbl.create !Config.default_hashtbl_sz
  ; funcs = Hashtbl.create !Config.default_hashtbl_sz
  ; macros = Hashtbl.create !Config.default_hashtbl_sz
  }

module Parser = struct
  let parse_tdef (t : EType.tdef) (p : t) : unit =
    let tn = EType.tdef_name t in
    match Hashtbl.find_opt p.tdefs tn.it with
    | None -> Hashtbl.replace p.tdefs tn.it t
    | Some _ -> Eslerr.(compile ~src:(ErrSrc.at tn) (DuplicatedTdef tn.it))

  let parse_func (f : EFunc.t) (p : t) : unit =
    let fn = EFunc.name f in
    match Hashtbl.find_opt p.funcs fn.it with
    | None -> Hashtbl.replace p.funcs fn.it f
    | Some _ -> Eslerr.(compile ~src:(ErrSrc.at fn) (DuplicatedFunc fn.it))

  let parse_macro (m : EMacro.t) (p : t) : unit =
    let mn = EMacro.name m in
    match Hashtbl.find_opt p.macros mn.it with
    | None -> Hashtbl.replace p.macros mn.it m
    | Some _ -> Eslerr.(compile ~src:(ErrSrc.at mn) (DuplicatedMacro mn.it))

  let parse_prog (imports : Id.t list) (el_parsers : (t -> unit) list) : t =
    let p = { (default ()) with imports } in
    List.iter (fun el_parser -> el_parser p) el_parsers;
    p
end

let create (file : string) (imports : Id.t list) (tdefs : EType.tdef list)
  (funcs : EFunc.t list) (macros : EMacro.t list) : t =
  let p = { (default ()) with file; imports } in
  List.iter (fun t -> Parser.parse_tdef t p) tdefs;
  List.iter (fun f -> Parser.parse_func f p) funcs;
  List.iter (fun m -> Parser.parse_macro m p) macros;
  p

let file (p : t) : string = p.file
let imports (p : t) : Id.t list = p.imports
let tdefs (p : t) : (Id.t', EType.tdef) Hashtbl.t = p.tdefs
let funcs (p : t) : (Id.t', EFunc.t) Hashtbl.t = p.funcs
let macros (p : t) : (Id.t', EMacro.t) Hashtbl.t = p.macros

let pp (fmt : Fmt.t) (p : t) : unit =
  let open Fmt in
  let pp_import fmt import = fprintf fmt "import %a\n" Id.pp import in
  let pp_tdef fmt (_, t) = fprintf fmt "%a\n" EType.tdef_pp t in
  let pp_func fmt (_, f) = fprintf fmt "\n%a" EFunc.pp f in
  let pp_macro fmt (_, m) = fprintf fmt "\n%a" EMacro.pp m in
  fprintf fmt "%a\n%a%a%a" (pp_lst "" pp_import) p.imports
    (pp_hashtbl "" pp_tdef) p.tdefs (pp_hashtbl "\n" pp_func) p.funcs
    (pp_hashtbl "\n" pp_macro) p.macros

let str (p : t) : string = Fmt.asprintf "%a" pp p

let tdefs_lst (p : t) : EType.tdef list =
  Hashtbl.fold (fun _ t acc -> t :: acc) p.tdefs []

let funcs_lst (p : t) : EFunc.t list =
  Hashtbl.fold (fun _ f acc -> f :: acc) p.funcs []

let macros_lst (p : t) : EMacro.t list =
  Hashtbl.fold (fun _ m acc -> m :: acc) p.macros []

let apply_macros (p : t) : t =
  let find_macro_f = Hashtbl.find_opt p.macros in
  let apply_macro_f _ f acc = EFunc.apply_macros find_macro_f f :: acc in
  let funcs = Hashtbl.fold apply_macro_f p.funcs [] in
  create p.file p.imports (tdefs_lst p) funcs []

let lambdas (p : t) : (string * Id.t list * Id.t list * EStmt.t) list =
  Hashtbl.fold (fun _ f acc -> EFunc.lambdas f @ acc) p.funcs []
