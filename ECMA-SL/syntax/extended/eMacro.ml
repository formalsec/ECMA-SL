open EslBase
open Source

type t = t' Source.t

and t' =
  { name : Id.t
  ; params : Id.t list
  ; body : EStmt.t
  }

let default () : t =
  { name = Id.default (); params = []; body = EStmt.default () } @> none
[@@inline]

let create (name : Id.t) (params : Id.t list) (body : EStmt.t) : t' =
  { name; params; body }
[@@inline]

let name (macro : t) : Id.t = macro.it.name [@@inline]
let name' (macro : t) : Id.t' = (name macro).it
let params (macro : t) : Id.t list = macro.it.params [@@inline]
let params' (macro : t) : Id.t' list = List.map (fun px -> px.it) (params macro)
let body (macro : t) : EStmt.t = macro.it.body [@@inline]

let pp_signature (ppf : Fmt.t) (macro : t) : unit =
  let pp_params ppf pxs = Fmt.(pp_lst !>", " Id.pp) ppf pxs in
  Fmt.fmt ppf "macro %a(%a)" Id.pp macro.it.name pp_params macro.it.params

let pp_simple (ppf : Fmt.t) (macro : t) : unit =
  Fmt.fmt ppf "%a { ..." pp_signature macro

let pp (ppf : Fmt.t) (macro : t) : unit =
  Fmt.fmt ppf "%a %a" pp_signature macro EStmt.pp macro.it.body

let str (macro : t) : string = Fmt.str "%a" pp macro [@@inline]
