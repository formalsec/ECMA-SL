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
  let sort_f (x1, _) (x2, _) = String.compare x1 x2 in
  let store_binds = Hashtbl.to_seq store |> List.of_seq |> List.sort sort_f in
  fprintf fmt "%a" (pp_lst sep pp_binding) store_binds

let pp_inline (pp_val : 'a pp_fmt) (fmt : Fmt.formatter) (store : 'a t) :
  unit =
  let open Fmt in
  let pp_bind fmt (x, v) = fprintf fmt "%s: %a" x pp_val v in
  fprintf fmt "{ %a }" (pp pp_bind ", ") store

let pp_table (pp_val : 'a pp_fmt) (fmt : Fmt.formatter) (store : 'a t) : unit
    =
  let open Fmt in
  let max_f acc n = if n > acc then n else acc in
  let lengths = Hashtbl.to_seq_keys store |> Seq.map String.length in
  let max = Seq.fold_left max_f 0 lengths in
  let var_sep x = String.make (max - String.length x) ' ' in
  let pp_bind fmt (x, v) = fprintf fmt "%s%s | %a\n" (var_sep x) x pp_val v in
  pp pp_bind "" fmt store

let str ?(tabular : bool = true) (pp_val : 'a pp_fmt) (store : 'a t) : string =
  if tabular then Fmt.asprintf "%a" (pp_table pp_val) store
  else Fmt.asprintf "%a" (pp_inline pp_val) store
