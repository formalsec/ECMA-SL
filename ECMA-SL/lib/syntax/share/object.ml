type 'a t = (Field.t, 'a) Hashtbl.t

let create () : 'a t = Hashtbl.create !Flags.default_hashtbl_sz
let clone (obj : 'a t) = Hashtbl.copy obj
let get (obj : 'a t) (f : Field.t) : 'a option = Hashtbl.find_opt obj f
let set (obj : 'a t) (f : Field.t) (v : 'a) : unit = Hashtbl.replace obj f v
let delete (obj : 'a t) (f : Field.t) : unit = Hashtbl.remove obj f

let to_string (o : 'a t) (printer : 'a -> string) : string =
  let str_obj =
    Hashtbl.fold
      (fun n v ac ->
        (if ac <> "{ " then ac ^ ", " else ac)
        ^ Printf.sprintf "%s: %s" (Field.str n) (printer v))
      o "{ "
  in
  str_obj ^ " }"

let to_json (o : 'a t) (printer : 'a -> string) : string =
  let str_obj =
    Hashtbl.fold
      (fun n v ac ->
        (if ac <> "{ " then ac ^ ", " else ac)
        ^ Printf.sprintf "\"%s\": %s" (Field.str n) (printer v))
      o "{ "
  in
  str_obj ^ " }"

let to_list (obj : 'a t) : (Field.t * 'a) list =
  List.of_seq (Hashtbl.to_seq obj)

let get_fields (obj : 'a t) : Field.t list =
  List.of_seq (Hashtbl.to_seq_keys obj)
