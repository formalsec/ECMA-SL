type t = (Field.t, Val.t) Hashtbl.t

let create () : t = Hashtbl.create 511

let get (obj : t) (f : Field.t) : Val.t option = Hashtbl.find_opt obj f

let set (obj : t) (f : Field.t) (v : Val.t) : unit = Hashtbl.replace obj f v

let delete (obj : t) (f : Field.t) : unit = Hashtbl.remove obj f

let to_list (obj : t) : (Field.t * Val.t) list = List.of_seq (Hashtbl.to_seq obj)

let get_fields (obj : t) : Field.t list = List.of_seq (Hashtbl.to_seq_keys obj)

let str (obj : t) : string = (Hashtbl.fold (fun n v ac -> (if ac <> "{ " then ac ^ ", " else ac) ^ (Printf.sprintf "%s: %s" (Field.str n) (Val.str ~flt_with_dot:false v))) obj "{ ") ^ " }"

let to_json (obj : t) : string = (Hashtbl.fold (fun n v ac -> (if ac <> "{ " then ac ^ ", " else ac) ^ (Printf.sprintf "\"%s\": %s" (Field.str n) (Val.str ~flt_with_dot:false v))) obj "{ ") ^ " }"
