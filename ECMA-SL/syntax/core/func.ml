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

let create (name : Id.t) (params : Id.t list) (body : Stmt.t) : t' =
  { name; params; body }

let name (func : t) : Id.t = func.it.name
let name' (func : t) : Id.t' = func.it.name.it
let params (func : t) : Id.t list = func.it.params
let params' (func : t) : Id.t' list = List.map (fun px -> px.it) func.it.params
let body (func : t) : Stmt.t = func.it.body

let pp (ppf : Fmt.t) (func : t) : unit =
  Fmt.fmt ppf "function %a(%a) %a" Id.pp func.it.name
    Fmt.(pp_lst !>", " Id.pp)
    func.it.params Stmt.pp func.it.body

let pp_simple (ppf : Fmt.t) (func : t) : unit =
  Fmt.fmt ppf "function %a(%a) { ..." Id.pp func.it.name
    Fmt.(pp_lst !>", " Id.pp)
    func.it.params

let str (func : t) : string = Fmt.str "%a" pp func
