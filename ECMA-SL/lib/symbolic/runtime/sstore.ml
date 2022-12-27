module Symbolic_store = Map.Make(String)

type t = Sval.t Symbolic_store.t

let create (values : (string * Sval.t) list) : t =
  List.fold_left 
    (fun sto (x, v) -> Symbolic_store.add x v sto)
    Symbolic_store.empty values

let find_opt (store : t) (x : string) : Sval.t option =
  Symbolic_store.find_opt x store

let add (store : t) (x : string) (v : Sval.t) : t =
  Symbolic_store.add x v store

let to_string (store : t) : string =
  Symbolic_store.fold
    (fun n v ac ->
      (if ac <> "{ " then ac ^ ", " else ac)
      ^ Printf.sprintf "%s: %s" n (Sval.str v))
    store "{ "
  ^ " }"
