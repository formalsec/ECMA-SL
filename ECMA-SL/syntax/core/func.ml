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

let pp (fmt : Format.formatter) (func : t) : unit =
  let open Format in
  let { name; params; body } = func.it in
  let pp_sep seq fmt () = pp_print_string fmt seq in
  let pp_lst seq pp fmt lst = pp_print_list ~pp_sep:(pp_sep seq) pp fmt lst in
  fprintf fmt "function %s(%a) {\n%a\n}" name
    (pp_lst ", " pp_print_string)
    params Stmt.pp body

let str (func : t) : string = Format.asprintf "%a" pp func
