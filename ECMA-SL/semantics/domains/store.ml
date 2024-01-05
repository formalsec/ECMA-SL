type var = string
type 'a t = (var, 'a) Hashtbl.t
type 'a pp_fmt = Format.formatter -> 'a -> unit

let create (var_vals : (var * 'a) list) : 'a t =
  List.to_seq var_vals |> Hashtbl.of_seq

let clone (store : 'a t) : 'a t = Hashtbl.copy store
let get_opt (store : 'a t) (x : var) : 'a option = Hashtbl.find_opt store x

let get (store : 'a t) (x : var) : ('a, string) Result.t =
  match get_opt store x with
  | Some v' -> Ok v'
  | None -> Error (Format.sprintf "Cannot find variable '%s'." x)

let set (store : 'a t) (x : var) (v : 'a) : unit = Hashtbl.replace store x v

let pp (pp_binding : Format.formatter -> var * 'a -> unit) (sep : string)
  (fmt : Format.formatter) (store : 'a t) : unit =
  let open Format in
  let sort_f (x1, _) (x2, _) = String.compare x1 x2 in
  let store_binds = Hashtbl.to_seq store |> List.of_seq |> List.sort sort_f in
  let pp_sep fmt () = pp_print_string fmt sep in
  let pp_lst pp fmt lst = pp_print_list ~pp_sep pp fmt lst in
  fprintf fmt "%a" (pp_lst pp_binding) store_binds

let pp_inline (pp_val : 'a pp_fmt) (fmt : Format.formatter) (store : 'a t) :
  unit =
  let open Format in
  let pp_bind fmt (x, v) = fprintf fmt "%s: %a" x pp_val v in
  fprintf fmt "{ %a }" (pp pp_bind ", ") store

let pp_table (pp_val : 'a pp_fmt) (fmt : Format.formatter) (store : 'a t) : unit
    =
  let open Format in
  let max_f acc n = if n > acc then n else acc in
  let lengths = Hashtbl.to_seq_keys store |> Seq.map String.length in
  let max = Seq.fold_left max_f 0 lengths in
  let var_sep x = String.make (max - String.length x) ' ' in
  let pp_bind fmt (x, v) = fprintf fmt "%s%s | %a\n" (var_sep x) x pp_val v in
  pp pp_bind "" fmt store

let str ?(tabular : bool = true) (pp_val : 'a pp_fmt) (store : 'a t) : string =
  if tabular then Format.asprintf "%a" (pp_table pp_val) store
  else Format.asprintf "%a" (pp_inline pp_val) store
