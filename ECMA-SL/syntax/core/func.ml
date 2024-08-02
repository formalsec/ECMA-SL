open EslBase
open Source

type t = t' Source.t

and t' =
  { fn : Id.t
  ; pxs : Id.t list
  ; s : Stmt.t
  }

let default : unit -> t =
  let dflt = { fn = Id.default (); pxs = []; s = Stmt.default () } @> none in
  fun () -> dflt

let create (fn : Id.t) (pxs : Id.t list) (s : Stmt.t) : t' = { fn; pxs; s }
[@@inline]

let name (f : t) : Id.t = f.it.fn [@@inline]
let name' (f : t) : Id.t' = (name f).it
let params (f : t) : Id.t list = f.it.pxs [@@inline]
let params' (f : t) : Id.t' list = List.map (fun px -> px.it) (params f)
let body (f : t) : Stmt.t = f.it.s [@@inline]

let pp_signature (ppf : Fmt.t) (f : t) : unit =
  let pp_pxs ppf pxs = Fmt.(pp_lst !>", " Id.pp) ppf pxs in
  Fmt.fmt ppf "function %a(%a)" Id.pp f.it.fn pp_pxs f.it.pxs

let pp_simple (ppf : Fmt.t) (f : t) : unit =
  Fmt.fmt ppf "%a { ..." pp_signature f

let pp (ppf : Fmt.t) (f : t) : unit =
  Fmt.fmt ppf "%a %a" pp_signature f Stmt.pp f.it.s

let str (f : t) : string = Fmt.str "%a" pp f [@@inline]
