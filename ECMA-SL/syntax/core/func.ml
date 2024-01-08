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

let pp_signature (fmt : Fmt.formatter) (func : t) : unit =
  let open Fmt in
  let pp_sep seq fmt () = pp_print_string fmt seq in
  let pp_lst seq pp fmt lst = pp_print_list ~pp_sep:(pp_sep seq) pp fmt lst in
  let { name; params; _ } = func.it in
  fprintf fmt "function %s(%a) {" name (pp_lst ", " pp_print_string) params

let pp (fmt : Fmt.formatter) (func : t) : unit =
  let open Fmt in
  let body = func.it.body in
  fprintf fmt "%a\n%a\n}" pp_signature func Stmt.pp body

let pp_simple (fmt : Fmt.formatter) (func : t) : unit =
  let open Fmt in
  fprintf fmt "%a ..." pp_signature func

let str ?(simple : bool = false) (func : t) : string =
  if simple then Fmt.asprintf "%a" pp_simple func else Fmt.asprintf "%a" pp func
