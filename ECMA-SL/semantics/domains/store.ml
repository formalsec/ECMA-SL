type var = string
type 'a t = (var, 'a) Hashtbl.t

let create (values : (var * 'a) list) : 'a t =
  let sto = Hashtbl.create !Config.default_hashtbl_sz in
  List.iter (fun (x, v) -> Hashtbl.replace sto x v) values;
  sto

let clone (s : 'a t) : 'a t = Hashtbl.copy s
let get (s : 'a t) (x : var) : 'a option = Hashtbl.find_opt s x
let set (s : 'a t) (x : var) (v : 'a) : unit = Hashtbl.replace s x v

let str (s : 'a t) (printer : 'a -> string) : string =
  Hashtbl.fold
    (fun x v ac ->
      (if ac <> "{ " then ac ^ ", " else ac)
      ^ Printf.sprintf "%s: %s" x (printer v) )
    s "{ "
  ^ " }"
