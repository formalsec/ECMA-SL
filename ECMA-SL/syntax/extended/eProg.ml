open EslBase
open Source

type t =
  { file : Id.t'
  ; path : Id.t'
  ; imports : EImport.t list
  ; tdefs : (Id.t', EType.TDef.t) Hashtbl.t
  ; macros : (Id.t', EMacro.t) Hashtbl.t
  ; funcs : (Id.t', EFunc.t) Hashtbl.t
  }

let default () : t =
  { file = ""
  ; path = ""
  ; imports = []
  ; tdefs = Hashtbl.create !Base.default_hashtbl_sz
  ; macros = Hashtbl.create !Base.default_hashtbl_sz
  ; funcs = Hashtbl.create !Base.default_hashtbl_sz
  }
[@@inline]

let create (file : Id.t') (path : string) (imports : EImport.t list)
  (tdefs : (Id.t', EType.TDef.t) Hashtbl.t) (funcs : (Id.t', EFunc.t) Hashtbl.t)
  (macros : (Id.t', EMacro.t) Hashtbl.t) : t =
  { file; path; imports; tdefs; funcs; macros }
[@@inline]

let file (prog : t) : Id.t' = prog.file [@@inline]
let path (prog : t) : string = prog.path [@@inline]
let imports (prog : t) : EImport.t list = prog.imports [@@inline]
let tdefs (prog : t) : (Id.t', EType.TDef.t) Hashtbl.t = prog.tdefs [@@inline]
let macros (prog : t) : (Id.t', EMacro.t) Hashtbl.t = prog.macros [@@inline]
let funcs (prog : t) : (Id.t', EFunc.t) Hashtbl.t = prog.funcs [@@inline]

let pp (ppf : Fmt.t) (prog : t) : unit =
  let pp_div ppf len = if len > 0 then Fmt.fmt ppf "@\n@\n" else () in
  let pp_bind pp_v ppf (_, v) = pp_v ppf v in
  let pp_list pp_v ppf vs = Fmt.(pp_lst !>"@\n" pp_v) ppf vs in
  let pp_tbl pp_v ppf vs = Fmt.(pp_hashtbl !>"@\n" (pp_bind pp_v)) ppf vs in
  let pp_tbl2 pp_v ppf vs = Fmt.(pp_hashtbl !>"@\n@\n" (pp_bind pp_v)) ppf vs in
  Fmt.fmt ppf "%a%a%a%a%a%a%a" (pp_list EImport.pp) prog.imports pp_div
    (List.length prog.imports) (pp_tbl EType.TDef.pp) prog.tdefs pp_div
    (Hashtbl.length prog.tdefs)
    (pp_tbl2 EMacro.pp) prog.macros pp_div
    (Hashtbl.length prog.macros)
    (pp_tbl2 EFunc.pp) prog.funcs

let str (prog : t) : string = Fmt.str "%a" pp prog [@@inline]

let lambdas (prog : t) : (at * Id.t' * Id.t list * Id.t list * EStmt.t) list =
  Hashtbl.fold (fun _ f acc -> EFunc.lambdas f @ acc) prog.funcs []
