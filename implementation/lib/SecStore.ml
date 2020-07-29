type t = (string, Level.t) Hashtbl.t

let create_store (varvals : (string * Level.t) list) : t =
  let sto : t = Hashtbl.create 511 in
  List.iter (fun (x,v) -> Hashtbl.add sto x v) varvals;
  sto

let get_store (sto:t) (var : string) : Level.t =
  Hashtbl.find sto var

let set_store (sto:t) (var: string) (v : Level.t): unit =
  Hashtbl.replace sto var v

let str_of_store (sto:t) : string =
  Hashtbl.fold
    (fun k v ac ->
       let kv_str = Printf.sprintf "(%s, %s)" k (Level.str v) in
       if (ac == "");
       then kv_str
       else (ac ^ ", " ^kv_str))
    sto
    ""
