open EslBase
open Source

type t = t' Source.phrase

and t' =
  { name : Id.t
  ; params : Id.t list
  ; body : Stmt.t
  }

let default () : t =
  { name = Id.default (); params = []; body = Stmt.default () } @> no_region

let create (name : Id.t) (params : Id.t list) (body : Stmt.t) : t' =
  { name; params; body }

let name (f : t) : Id.t = f.it.name
let name' (f : t) : Id.t' = f.it.name.it
let params (f : t) : Id.t list = f.it.params
let params' (f : t) : Id.t' list = List.map (fun px -> px.it) f.it.params
let body (f : t) : Stmt.t = f.it.body

let pp_signature (ppf : Fmt.t) (f : t) : unit =
  let open Fmt in
  let { name; params; _ } = f.it in
  fmt ppf "function %a(%a)" Id.pp name (pp_lst !>", " Id.pp) params

let pp (ppf : Fmt.t) (f : t) : unit =
  Fmt.fmt ppf "%a %a" pp_signature f Stmt.pp f.it.body

let pp_simple (ppf : Fmt.t) (f : t) : unit =
  Fmt.fmt ppf "%a {..." pp_signature f

let str ?(simple : bool = false) (f : t) : string =
  if simple then Fmt.str "%a" pp_simple f else Fmt.str "%a" pp f
