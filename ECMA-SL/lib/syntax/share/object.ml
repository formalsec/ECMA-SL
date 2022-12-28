type 'a t = (Field.t, 'a) Hashtbl.t

let create () : 'a t = Hashtbl.create Common.default_hashtable_size
let get (obj : 'a t) (f : Field.t) : 'a option = Hashtbl.find_opt obj f
let set (obj : 'a t) (f : Field.t) (v : 'a) : unit = Hashtbl.replace obj f v
let delete (obj : 'a t) (f : Field.t) : unit = Hashtbl.remove obj f

let to_list (obj : 'a t) : (Field.t * 'a) list =
  List.of_seq (Hashtbl.to_seq obj)

let get_fields (obj : 'a t) : Field.t list =
  List.of_seq (Hashtbl.to_seq_keys obj)
