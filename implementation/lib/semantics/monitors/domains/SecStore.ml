type t = (string, SecLevel.t) Hashtbl.t

let create (varvals : (string * SecLevel.t) list) : t =
  let sto : t = Hashtbl.create 511 in
  List.iter (fun (x,v) -> Hashtbl.replace sto x v) varvals;
  sto

let get (sto:t) (var : string) : SecLevel.t =
  Hashtbl.find sto var

let set (sto:t) (var: string) (v : SecLevel.t): unit =
  Hashtbl.replace sto var v

let get_safe (sto : t) (var : string) : SecLevel.t option =
  Hashtbl.find_opt sto var

let str (sto:t) : string =
  Hashtbl.fold
    (fun k v ac ->
       let kv_str = Printf.sprintf "(%s, %s)" k (SecLevel.str v) in
       if (ac == "");
       then kv_str
       else (ac ^ ", " ^kv_str))
    sto
    ""
