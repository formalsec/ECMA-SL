open Core

type 'a t = (String.t, 'a) Hashtbl.t

let create () : 'a t = Hashtbl.create (module String)
let clone (o : 'a t) = Hashtbl.copy o

let set (o : 'a t) (key : String.t) (data : 'a) : unit =
  Hashtbl.set o ~key ~data

let get (o : 'a t) (key : String.t) : 'a option = Hashtbl.find o key
let delete (o : 'a t) (f : String.t) : unit = Hashtbl.remove o f

let to_string (o : 'a t) (printer : 'a -> string) : string =
  let str_obj =
    Hashtbl.fold o ~init:"{ " ~f:(fun ~key:n ~data:v ac ->
        (if String.(ac <> "{ ") then ac ^ ", " else ac)
        ^ Printf.sprintf "\"%s\": %s" n (printer v) )
  in
  str_obj ^ " }"

let to_json (o : 'a t) (printer : 'a -> string) : string =
  let str_obj =
    Hashtbl.fold o ~init:"{ " ~f:(fun ~key:n ~data:v ac ->
        (if String.(ac <> "{ ") then ac ^ ", " else ac)
        ^ Printf.sprintf "\"%s\": %s" n (printer v) )
  in
  str_obj ^ " }"

let to_list (o : 'a t) : (String.t * 'a) list = Hashtbl.to_alist o
let get_fields (o : 'a t) : String.t list = Hashtbl.keys o
