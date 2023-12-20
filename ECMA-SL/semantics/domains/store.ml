type var = string
type 'a t = (var, 'a) Hashtbl.t

let create (var_vals : (var * 'a) list) : 'a t =
  List.to_seq var_vals |> Hashtbl.of_seq

let clone (store : 'a t) : 'a t = Hashtbl.copy store
let get (store : 'a t) (x : var) : 'a option = Hashtbl.find_opt store x
let set (store : 'a t) (x : var) (v : 'a) : unit = Hashtbl.replace store x v

let str (store : 'a t) (printer : 'a -> string) : string =
  let _binding_str x v = Printf.sprintf "%s: %s" x (printer v) in
  let _store_str_f x v acc = _binding_str x v :: acc in
  let store_str = Hashtbl.fold _store_str_f store [] |> String.concat ", " in
  "{ " ^ store_str ^ " }"
