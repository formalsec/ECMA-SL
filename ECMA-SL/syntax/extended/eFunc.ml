open Source

type t = t' Source.phrase

and t' =
  { name : string
  ; tparams : (string * EType.t option) list
  ; treturn : EType.t option
  ; body : EStmt.t
  ; metadata : EFunc_metadata.t option
  }

let default () : t' =
  { name = ""
  ; tparams = []
  ; treturn = None
  ; body = EStmt.default ()
  ; metadata = None
  }

let create (name : string) (tparams : (string * EType.t option) list)
  (treturn : EType.t option) (body : EStmt.t)
  (metadata : EFunc_metadata.t option) : t' =
  { name; tparams; treturn; body; metadata }

let name (f : t) : string = f.it.name
let tparams (f : t) : (string * EType.t option) list = f.it.tparams
let params (f : t) : string list = List.map fst f.it.tparams
let treturn (f : t) : EType.t option = f.it.treturn
let body (f : t) : EStmt.t = f.it.body
let metadata (f : t) : EFunc_metadata.t option = f.it.metadata

let pp_signature (fmt : Fmt.t) (f : t) : unit =
  let open Fmt in
  let { name; tparams; treturn; _ } = f.it in
  let pp_tparam fmt (param, t) = fprintf fmt "%s%a" param EType.pp_tannot t in
  fprintf fmt "function %s(%a)%a" name (pp_lst ", " pp_tparam) tparams
    EType.pp_tannot treturn

let pp (fmt : Fmt.t) (f : t) : unit =
  Fmt.fprintf fmt "%a %a" pp_signature f EStmt.pp f.it.body

let pp_simple (fmt : Fmt.t) (f : t) : unit =
  Fmt.fprintf fmt "%a {..." pp_signature f

let str ?(simple : bool = false) (f : t) : string =
  if simple then Fmt.asprintf "%a" pp_simple f else Fmt.asprintf "%a" pp f

let apply_macros (find_macro_f : string -> EMacro.t option) (f : t) : t =
  let body = EStmt.map (EMacro.mapper find_macro_f) f.it.body in
  { f with it = { f.it with body } }

(* FIXME: Requires cleaning below *)
let lambdas (f : t) : (string * string list * string list * EStmt.t) list =
  EStmt.lambdas f.it.body
