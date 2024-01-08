open Source

type t = t' Source.phrase

and t' =
  { name : string
  ; params : string list
  ; body : Stmt.t
  }

let name (f : t) : string = f.it.name
let params (f : t) : string list = f.it.params
let body (f : t) : Stmt.t = f.it.body

let create (name : string) (params : string list) (body : Stmt.t) : t' =
  { name; params; body }

let pp_signature (fmt : Fmt.formatter) (f : t) : unit =
  let open Fmt in
  let pp_sep seq fmt () = pp_print_string fmt seq in
  let pp_lst seq pp fmt lst = pp_print_list ~pp_sep:(pp_sep seq) pp fmt lst in
  let { name; params; _ } = f.it in
  fprintf fmt "function %s(%a) {" name (pp_lst ", " pp_print_string) params

let pp (fmt : Fmt.formatter) (f : t) : unit =
  let open Fmt in
  let body = f.it.body in
  fprintf fmt "%a\n%a\n}" pp_signature f Stmt.pp body

let pp_simple (fmt : Fmt.formatter) (f : t) : unit =
  let open Fmt in
  fprintf fmt "%a ..." pp_signature f

let str ?(simple : bool = false) (f : t) : string =
  if simple then Fmt.asprintf "%a" pp_simple f else Fmt.asprintf "%a" pp f
