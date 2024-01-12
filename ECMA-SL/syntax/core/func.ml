open Source

type t = t' Source.phrase

and t' =
  { name : string
  ; params : string list
  ; body : Stmt.t
  }

let name (func : t) : string = func.it.name
let params (func : t) : string list = func.it.params
let body (func : t) : Stmt.t = func.it.body

let create (name : string) (params : string list) (body : Stmt.t) : t' =
  { name; params; body }

let pp (fmt : Fmt.formatter) ({ it = { name; params; body }; _ }: t) : unit =
  let open Fmt in
  fprintf fmt "function %s(%a) {\n%a\n}" name (pp_lst ", " pp_str) params
    Stmt.pp body

let str (func : t) : string = Fmt.asprintf "%a" pp func
