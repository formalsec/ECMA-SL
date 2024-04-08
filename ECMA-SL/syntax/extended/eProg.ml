open EslBase
open Source

type import =
  [ `User of Id.t
  | `Standard of Id.t
  ]

type t =
  { file : Id.t'
  ; path : Id.t'
  ; imports : import list
  ; tdefs : (Id.t', EType.TDef.t) Hashtbl.t
  ; funcs : (Id.t', EFunc.t) Hashtbl.t
  ; macros : (Id.t', EMacro.t) Hashtbl.t
  }

let default () : t =
  { file = ""
  ; path = ""
  ; imports = []
  ; tdefs = Hashtbl.create !Base.default_hashtbl_sz
  ; funcs = Hashtbl.create !Base.default_hashtbl_sz
  ; macros = Hashtbl.create !Base.default_hashtbl_sz
  }

let create (file : Id.t') (path : string) (imports : import list)
  (tdefs : (Id.t', EType.TDef.t) Hashtbl.t) (funcs : (Id.t', EFunc.t) Hashtbl.t)
  (macros : (Id.t', EMacro.t) Hashtbl.t) : t =
  { file; path; imports; tdefs; funcs; macros }

let file (p : t) : Id.t' = p.file
let path (p : t) : string = p.path
let imports (p : t) : import list = p.imports
let tdefs (p : t) : (Id.t', EType.TDef.t) Hashtbl.t = p.tdefs
let funcs (p : t) : (Id.t', EFunc.t) Hashtbl.t = p.funcs
let macros (p : t) : (Id.t', EMacro.t) Hashtbl.t = p.macros

let pp (fmt : Fmt.t) (p : t) : unit =
  let open Fmt in
  let pp_import fmt = function
    | `User import -> fprintf fmt "import \"%a\"@\n" Id.pp import
    | `Standard import -> fprintf fmt "import %a@\n" Id.pp import
  in
  let pp_tdef fmt (_, t) = fprintf fmt "%a\n" EType.TDef.pp t in
  let pp_func fmt (_, f) = fprintf fmt "\n%a" EFunc.pp f in
  let pp_macro fmt (_, m) = fprintf fmt "\n%a" EMacro.pp m in
  fprintf fmt "%a\n%a%a%a" (pp_lst "" pp_import) p.imports
    (pp_hashtbl "" pp_tdef) p.tdefs (pp_hashtbl "\n" pp_func) p.funcs
    (pp_hashtbl "\n" pp_macro) p.macros

let str (p : t) : string = Fmt.asprintf "%a" pp p

let lambdas (p : t) : (region * Id.t' * Id.t list * Id.t list * EStmt.t) list =
  Hashtbl.fold (fun _ f acc -> EFunc.lambdas f @ acc) p.funcs []
