open EslBase
open Source
module Meta = EFunc_metadata

type t = t' Source.phrase

and t' =
  { name : Id.t
  ; tparams : (Id.t * EType.t option) list
  ; treturn : EType.t option
  ; body : EStmt.t
  ; metadata : Meta.t option
  }

let default () : t =
  { name = Id.default ()
  ; tparams = []
  ; treturn = None
  ; body = EStmt.default ()
  ; metadata = None
  }
  @> no_region

let create (name : Id.t) (tparams : (Id.t * EType.t option) list)
  (treturn : EType.t option) (body : EStmt.t) (metadata : Meta.t option) : t' =
  { name; tparams; treturn; body; metadata }

let name (m : t) : Id.t = m.it.name
let name' (m : t) : Id.t' = m.it.name.it
let tparams (f : t) : (Id.t * EType.t option) list = f.it.tparams
let params (f : t) : Id.t list = List.map (fun (px, _) -> px) f.it.tparams
let params' (m : t) : Id.t' list = List.map (fun (px, _) -> px.it) m.it.tparams
let treturn (f : t) : EType.t option = f.it.treturn
let body (f : t) : EStmt.t = f.it.body
let metadata (f : t) : Meta.t option = f.it.metadata

let pp_signature (fmt : Fmt.t) (f : t) : unit =
  let open Fmt in
  let { name; tparams; treturn; _ } = f.it in
  let pp_param fmt (px, t) = fprintf fmt "%a%a" Id.pp px EType.tannot_pp t in
  fprintf fmt "function %a(%a)%a" Id.pp name (pp_lst ", " pp_param) tparams
    EType.tannot_pp treturn

let pp (fmt : Fmt.t) (f : t) : unit =
  Fmt.fprintf fmt "%a %a" pp_signature f EStmt.pp f.it.body

let pp_simple (fmt : Fmt.t) (f : t) : unit =
  Fmt.fprintf fmt "%a {..." pp_signature f

let str ?(simple : bool = false) (f : t) : string =
  Fmt.asprintf "%a" (if simple then pp_simple else pp) f

let lambdas (f : t) : (region * Id.t' * Id.t list * Id.t list * EStmt.t) list =
  let to_list_f s =
    match s.it with
    | EStmt.Lambda (_, id, pxs, ctxvars, s) -> [ (s.at, id, pxs, ctxvars, s) ]
    | _ -> []
  in
  EStmt.to_list ~recursion:true to_list_f (body f)
