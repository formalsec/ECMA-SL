type var = string
type 'a t = (var, 'a) Hashtbl.t
type 'a pp_fmt = Fmt.formatter -> 'a -> unit

let create (var_vals : (var * 'a) list) : 'a t =
  List.to_seq var_vals |> Hashtbl.of_seq

let clone (store : 'a t) : 'a t = Hashtbl.copy store
let get_opt (store : 'a t) (x : var) : 'a option = Hashtbl.find_opt store x

let get (store : 'a t) (x : var) : ('a, string) Result.t =
  match get_opt store x with
  | Some v' -> Ok v'
  | None -> Error (Fmt.sprintf "Cannot find variable '%s'." x)

let set (store : 'a t) (x : var) (v : 'a) : unit = Hashtbl.replace store x v

let pp (pp_binding : Fmt.formatter -> var * 'a -> unit) (sep : string)
  (fmt : Fmt.formatter) (store : 'a t) : unit =
  let open Fmt in
  let pp_sep fmt () = pp_print_string fmt sep in
  let pp_seq pp fmt lst = pp_print_seq ~pp_sep pp fmt lst in
  fprintf fmt "%a" (pp_seq pp_binding) (Hashtbl.to_seq store)

let pp_inline (pp_val : 'a pp_fmt) (fmt : Fmt.formatter) (store : 'a t) : unit =
  let open Fmt in
  let pp_bind fmt (x, v) = fprintf fmt "%s: %a" x pp_val v in
  fprintf fmt "{ %a }" (pp pp_bind ", ") store

let pp_table (pp_val : 'a pp_fmt) (fmt : Fmt.formatter) (store : 'a t) : unit =
  let open Fmt in
  let lengths = Hashtbl.to_seq_keys store |> Seq.map String.length in
  let max = Seq.fold_left (fun acc n -> if n > acc then n else acc) 0 lengths in
  let var_sep x = String.make (max - String.length x) ' ' in
  let pp_binding fmt (x, v) =
    fprintf fmt "» %s%s  <-  %a\n" (var_sep x) x pp_val v
  in
  pp pp_binding "" fmt store

let str ?(tabular : bool = false) (pp_val : 'a pp_fmt) (store : 'a t) : string =
  if tabular then Fmt.asprintf "%a" (pp_table pp_val) store
  else Fmt.asprintf "%a" (pp_inline pp_val) store
