open EslBase
open Source

type t =
  { file : Id.t'
  ; path : Id.t'
  ; imports : EImport.t list
  ; tdefs : (Id.t', EType.TDef.t) Hashtbl.t
  ; macros : (Id.t', EMacro.t) Hashtbl.t
  ; funcs : (Id.t', EFunc.t) Hashtbl.t
  ; advices : (Id.t', EAdvice.t list) Hashtbl.t
  }

let default () : t =
  { file = ""
  ; path = ""
  ; imports = []
  ; tdefs = Hashtbl.create !Base.default_hashtbl_sz
  ; macros = Hashtbl.create !Base.default_hashtbl_sz
  ; funcs = Hashtbl.create !Base.default_hashtbl_sz
  ; advices = Hashtbl.create !Base.default_hashtbl_sz
  }

let create (file : Id.t') (path : string) (imports : EImport.t list)
  (tdefs : (Id.t', EType.TDef.t) Hashtbl.t) (funcs : (Id.t', EFunc.t) Hashtbl.t)
  (macros : (Id.t', EMacro.t) Hashtbl.t)
  (advices : (Id.t', EAdvice.t list) Hashtbl.t) : t =
  { file; path; imports; tdefs; funcs; macros; advices }
[@@inline]

let file (p : t) : Id.t' = p.file [@@inline]
let path (p : t) : string = p.path [@@inline]
let imports (p : t) : EImport.t list = p.imports [@@inline]
let tdefs (p : t) : (Id.t', EType.TDef.t) Hashtbl.t = p.tdefs [@@inline]
let macros (p : t) : (Id.t', EMacro.t) Hashtbl.t = p.macros [@@inline]
let funcs (p : t) : (Id.t', EFunc.t) Hashtbl.t = p.funcs [@@inline]
let advices (p : t) : (Id.t', EAdvice.t list) Hashtbl.t = p.advices [@@inline]

let pp (ppf : Format.formatter) (p : t) : unit =
  let newline ppf () = Fmt.pf ppf "@\n" in
  let pp_div ppf len = if len > 0 then Fmt.pf ppf "@\n@\n" else () in
  let pp_bind pp_v ppf (_, v) = pp_v ppf v in
  let pp_list pp_v ppf vs = Fmt.(list ~sep:newline pp_v) ppf vs in
  let pp_tbl pp_v ppf vs = Fmt.(hashtbl ~sep:newline (pp_bind pp_v)) ppf vs in
  let pp_tbl2 pp_v ppf vs =
    Fmt.(hashtbl ~sep:(fun fmt () -> Fmt.pf fmt "@\n@\n") (pp_bind pp_v)) ppf vs
  in
  let pp_tbl3 pp_v ppf vs =
    Fmt.(hashtbl ~sep:newline (fun ppf (_, vs) -> pp_list pp_v ppf vs)) ppf vs
  in
  Fmt.pf ppf "%a%a%a%a%a%a%a%a" (pp_list EImport.pp) p.imports pp_div
    (List.length p.imports) (pp_tbl EType.TDef.pp) p.tdefs pp_div
    (Hashtbl.length p.tdefs) (pp_tbl2 EMacro.pp) p.macros pp_div
    (Hashtbl.length p.macros) (pp_tbl2 EFunc.pp) p.funcs (pp_tbl3 EAdvice.pp)
    p.advices

let str (p : t) : string = Fmt.str "%a" pp p [@@inline]

let lambdas (p : t) : (at * Id.t' * Id.t list * Id.t list * EStmt.t) list =
  Hashtbl.fold (fun _ f acc -> EFunc.lambdas f @ acc) p.funcs []
