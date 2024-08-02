open EslBase
open Source

type t = t' Source.t

and t' =
  { mn : Id.t
  ; pxs : Id.t list
  ; s : EStmt.t
  }

let default : unit -> t =
  let dflt = { mn = Id.default (); pxs = []; s = EStmt.default () } @> none in
  fun () -> dflt

let create (mn : Id.t) (pxs : Id.t list) (s : EStmt.t) : t' = { mn; pxs; s }
[@@inline]

let name (m : t) : Id.t = m.it.mn [@@inline]
let name' (m : t) : Id.t' = (name m).it
let params (m : t) : Id.t list = m.it.pxs [@@inline]
let params' (m : t) : Id.t' list = List.map (fun px -> px.it) (params m)
let body (m : t) : EStmt.t = m.it.s [@@inline]

let pp_signature (ppf : Fmt.t) (m : t) : unit =
  let pp_pxs ppf pxs = Fmt.(pp_lst !>", " Id.pp) ppf pxs in
  Fmt.fmt ppf "macro %a(%a)" Id.pp m.it.mn pp_pxs m.it.pxs

let pp_simple (ppf : Fmt.t) (m : t) : unit =
  Fmt.fmt ppf "%a { ..." pp_signature m

let pp (ppf : Fmt.t) (m : t) : unit =
  Fmt.fmt ppf "%a %a" pp_signature m EStmt.pp m.it.s

let str (m : t) : string = Fmt.str "%a" pp m [@@inline]
