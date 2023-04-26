open Core

type 'a t = { parent : 'a t option; map : (String.t, 'a) Hashtbl.t }

let create () : 'a t = { parent = None; map = Hashtbl.create (module String) }
let clone (o : 'a t) = { parent = Some o; map = Hashtbl.create (module String) }

let rec get (o : 'a t) (key : String.t) : 'a option =
  match Hashtbl.find o.map key with
  | Some _ as v -> v
  | None -> Option.bind o.parent ~f:(fun o -> get o key)

let set (o : 'a t) (key : String.t) (data : 'a) : unit =
  Hashtbl.set o.map ~key ~data

let delete (o : 'a t) (f : String.t) : unit = Hashtbl.remove o.map f

let to_string (o : 'a t) (printer : 'a -> string) : string =
  let str_obj =
    Hashtbl.fold o.map ~init:"{ " ~f:(fun ~key:n ~data:v ac ->
        (if String.(ac <> "{ ") then ac ^ ", " else ac)
        ^ Printf.sprintf "\"%s\": %s" n (printer v))
  in
  str_obj ^ " }"

let to_json (o : 'a t) (printer : 'a -> string) : string =
  let str_obj =
    Hashtbl.fold o.map ~init:"{ " ~f:(fun ~key:n ~data:v ac ->
        (if String.(ac <> "{ ") then ac ^ ", " else ac)
        ^ Printf.sprintf "\"%s\": %s" n (printer v))
  in
  str_obj ^ " }"

let to_list (o : 'a t) : (String.t * 'a) list = Hashtbl.to_alist o.map
let get_fields (o : 'a t) : String.t list = Hashtbl.keys o.map
