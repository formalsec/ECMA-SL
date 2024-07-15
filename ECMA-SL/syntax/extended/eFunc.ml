open EslBase
open Source

type t = t' Source.t

and t' =
  { name : Id.t
  ; tparams : (Id.t * EType.t option) list
  ; treturn : EType.t option
  ; body : EStmt.t
  }

let default () : t =
  let name = Id.default () in
  let body = EStmt.default () in
  { name; tparams = []; treturn = None; body } @> none

let create (name : Id.t) (tparams : (Id.t * EType.t option) list)
  (treturn : EType.t option) (body : EStmt.t) : t' =
  { name; tparams; treturn; body }

let name (m : t) : Id.t = m.it.name
let name' (m : t) : Id.t' = m.it.name.it
let tparams (f : t) : (Id.t * EType.t option) list = f.it.tparams
let params (f : t) : Id.t list = List.map (fun (px, _) -> px) f.it.tparams
let params' (m : t) : Id.t' list = List.map (fun (px, _) -> px.it) m.it.tparams
let treturn (f : t) : EType.t option = f.it.treturn
let body (f : t) : EStmt.t = f.it.body

let pp_signature (ppf : Fmt.t) (f : t) : unit =
  let open Fmt in
  let { name; tparams; treturn; _ } = f.it in
  let pp_param ppf (px, t) = fmt ppf "%a%a" Id.pp px EType.tannot_pp t in
  fmt ppf "function %a(%a)%a" Id.pp name (pp_lst !>", " pp_param) tparams
    EType.tannot_pp treturn

let pp (ppf : Fmt.t) (f : t) : unit =
  Fmt.fmt ppf "%a %a" pp_signature f EStmt.pp f.it.body

let pp_simple (ppf : Fmt.t) (f : t) : unit =
  Fmt.fmt ppf "%a {..." pp_signature f

let str ?(simple : bool = false) (f : t) : string =
  Fmt.str "%a" (if simple then pp_simple else pp) f

let lambdas (f : t) : (at * Id.t' * Id.t list * Id.t list * EStmt.t) list =
  let to_list_f s =
    match s.it with
    | EStmt.Lambda (_, id, pxs, ctxvars, s) -> [ (s.at, id, pxs, ctxvars, s) ]
    | _ -> []
  in
  EStmt.to_list ~recursion:true to_list_f (body f)
