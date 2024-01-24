open Source

type t = t' Source.phrase

and t' =
  { name : string
  ; params : string list
  ; body : Stmt.t
  }

let default () : t =
  { name = ""; params = []; body = Stmt.default () } @> Source.no_region

let create (name : string) (params : string list) (body : Stmt.t) : t' =
  { name; params; body }

let name (f : t) : string = f.it.name
let params (f : t) : string list = f.it.params
let body (f : t) : Stmt.t = f.it.body

let pp_signature (fmt : Fmt.t) (f : t) : unit =
  let open Fmt in
  let { name; params; _ } = f.it in
  fprintf fmt "function %s(%a)" name (pp_lst ", " pp_str) params

let pp (fmt : Fmt.t) (f : t) : unit =
  Fmt.fprintf fmt "%a %a" pp_signature f Stmt.pp f.it.body

let pp_simple (fmt : Fmt.t) (f : t) : unit =
  Fmt.fprintf fmt "%a {..." pp_signature f

let str ?(simple : bool = false) (f : t) : string =
  if simple then Fmt.asprintf "%a" pp_simple f else Fmt.asprintf "%a" pp f
