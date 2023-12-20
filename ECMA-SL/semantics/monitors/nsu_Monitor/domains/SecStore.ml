type 'sl t = (string, 'sl) Hashtbl.t

let create (varvals : (string * 'sl) list) : 'sl t =
  let sto : 'sl t = Hashtbl.create 511 in
  List.iter (fun (x, v) -> Hashtbl.replace sto x v) varvals;
  sto

let get (sto : 'sl t) (var : string) : 'sl = Hashtbl.find sto var

let set (sto : 'sl t) (var : string) (v : 'sl) : unit =
  Hashtbl.replace sto var v

let get_safe (sto : 'sl t) (var : string) : 'sl option =
  Hashtbl.find_opt sto var

let str (str_sl : 'sl -> string) (sto : 'sl t) : string =
  Hashtbl.fold
    (fun k v ac ->
      let kv_str = Printf.sprintf "(%s, %s)" k (str_sl v) in
      if ac == "" then kv_str else ac ^ ", " ^ kv_str )
    sto ""
