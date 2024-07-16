open EslBase
open Source

type t = t' Source.t

and t' =
  { name : Id.t
  ; params : Id.t list
  ; body : Stmt.t
  }

let default () : t =
  { name = Id.default (); params = []; body = Stmt.default () } @> none
[@@inline]

let create (name : Id.t) (params : Id.t list) (body : Stmt.t) : t' =
  { name; params; body }
[@@inline]

let name (func : t) : Id.t = func.it.name [@@inline]
let name' (func : t) : Id.t' = (name func).it
let params (func : t) : Id.t list = func.it.params [@@inline]
let params' (func : t) : Id.t' list = List.map (fun px -> px.it) (params func)
let body (func : t) : Stmt.t = func.it.body [@@inline]

let pp_signature (ppf : Fmt.t) (func : t) : unit =
  let pp_params ppf pxs = Fmt.(pp_lst !>", " Id.pp) ppf pxs in
  Fmt.fmt ppf "function %a(%a)" Id.pp func.it.name pp_params func.it.params

let pp_simple (ppf : Fmt.t) (func : t) : unit =
  Fmt.fmt ppf "%a { ..." pp_signature func

let pp (ppf : Fmt.t) (func : t) : unit =
  Fmt.fmt ppf "%a %a" pp_signature func Stmt.pp func.it.body

let str (func : t) : string = Fmt.str "%a" pp func [@@inline]
