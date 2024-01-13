type var = string
type 'a t = (var, 'a) Hashtbl.t

let create (var_vals : (var * 'a) list) : 'a t =
  List.to_seq var_vals |> Hashtbl.of_seq

let clone (store : 'a t) : 'a t = Hashtbl.copy store
let get_opt (store : 'a t) (x : var) : 'a option = Hashtbl.find_opt store x

let get (store : 'a t) (x : var) : ('a, string) Result.t =
  match get_opt store x with
  | Some v' -> Ok v'
  | None -> Error (Fmt.sprintf "Cannot find variable '%s'." x)

let set (store : 'a t) (x : var) (v : 'a) : unit = Hashtbl.replace store x v

let pp (pp_val : Fmt.t -> 'a -> unit) (fmt : Fmt.t) (store : 'a t) : unit =
  let open Fmt in
  let pp_binding fmt x v = fprintf fmt "%s: %a" x pp_val v in
  if Hashtbl.length store = 0 then pp_str fmt "{}"
  else fprintf fmt "{ %a }" (pp_hashtbl ", " pp_binding) store

let pp_tabular (pp_val : Fmt.t -> 'a -> unit) (fmt : Fmt.t) (store : 'a t) :
  unit =
  let open Fmt in
  let lengths = Hashtbl.to_seq_keys store |> Seq.map String.length in
  let max = Seq.fold_left Int.max 0 lengths in
  let indent x = String.make (max - String.length x) ' ' in
  let pp_binding fmt x v = fprintf fmt "%s%s  <-  %a" (indent x) x pp_val v in
  fprintf fmt "%a" (pp_hashtbl "\n" pp_binding) store

let str ?(tabular : bool = false) (pp_val : Fmt.t -> 'a -> unit) (store : 'a t)
  : string =
  if tabular then Fmt.asprintf "%a" (pp_tabular pp_val) store
  else Fmt.asprintf "%a" (pp pp_val) store
