type var = string
type 'a t = (var, 'a) Hashtbl.t

let create (var_vals : (var * 'a) list) : 'a t =
  List.to_seq var_vals |> Hashtbl.of_seq

let clone (store : 'a t) : 'a t = Hashtbl.copy store
let get_opt (store : 'a t) (x : var) : 'a option = Hashtbl.find_opt store x

let get (store : 'a t) (x : var) : ('a, string) Result.t =
  match get_opt store x with
  | Some v' -> Ok v'
  | None -> Error (Format.sprintf "Cannot find variable '%s'." x)

let set (store : 'a t) (x : var) (v : 'a) : unit = Hashtbl.replace store x v

let str (val_printer : Val.t -> string) (store : 'a t) : string =
  let _str_binding x v = Printf.sprintf "%s: %s" x (val_printer v) in
  let _str_store_f x v acc = _str_binding x v :: acc in
  let store_str = Hashtbl.fold _str_store_f store [] |> String.concat ", " in
  "{ " ^ store_str ^ " }"
