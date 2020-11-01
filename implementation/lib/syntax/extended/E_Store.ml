type t = (string, Val.t) Hashtbl.t

let create (varvals : (string * Val.t) list) : t =
  let sto = Hashtbl.create 511 in
  List.iter (fun (x, v) -> Hashtbl.replace sto x v) varvals;
  sto

let get (sto : t) (name : string) : Val.t option = Hashtbl.find_opt sto name

let set (sto : t) (name : string) (value : Val.t) : unit = Hashtbl.replace sto name value

let str (sto : t) : string = (Hashtbl.fold (fun n v ac -> (if ac <> "{ " then ac ^ ", " else ac) ^ (Printf.sprintf "%s: %s" n (Val.str v))) sto "{ ") ^ " }"
