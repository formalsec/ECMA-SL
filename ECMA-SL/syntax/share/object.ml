open Core

type 'a t = (String.t, 'a) Hashtbl.t

let create () : 'a t = Hashtbl.create (module String)
let clone (obj : 'a t) = Hashtbl.copy obj
let fld_list (obj : 'a t) : (String.t * 'a) list = Hashtbl.to_alist obj
let flds (obj : 'a t) : String.t list = Hashtbl.keys obj
let get (obj : 'a t) (key : String.t) : 'a option = Hashtbl.find obj key
let delete (obj : 'a t) (f : String.t) : unit = Hashtbl.remove obj f

let set (obj : 'a t) (key : String.t) (data : 'a) : unit =
  Hashtbl.set obj ~key ~data

let str (obj : 'a t) (printer : 'a -> string) : string =
  let print_fun (key, data) = Printf.sprintf "\"%s\": %s" key (printer data) in
  let obj_flds_str = Hashtbl.to_alist obj |> List.map ~f:print_fun in
  "{ " ^ String.concat ~sep:", " obj_flds_str ^ "} "

let to_json (obj : 'a t) (printer : 'a -> string) : string =
  let print_fun (key, data) = Printf.sprintf "\"%s\": %s" key (printer data) in
  let obj_flds_str = Hashtbl.to_alist obj |> List.map ~f:print_fun in
  "{ " ^ String.concat ~sep:", " obj_flds_str ^ "} "
