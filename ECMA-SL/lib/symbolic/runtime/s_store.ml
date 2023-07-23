module Symbolic_store = Map.Make (String)

type bind = String.t
type t = Expr.t Symbolic_store.t

let create (values : (bind * Expr.t) list) : t =
  List.fold_left
    (fun sto (key, data) -> Symbolic_store.add key data sto)
    Symbolic_store.empty values

let mem (store : t) (x : bind) : bool = Symbolic_store.mem x store

let add_exn (store : t) (key : bind) (data : Expr.t) : t =
  Symbolic_store.add key data store

let find (store : t) (x : bind) : Expr.t option =
  Symbolic_store.find_opt x store

let to_string (store : t) : string =
  let start = "{ ... " in
  Symbolic_store.fold
    (fun key data accum ->
      if String.starts_with ~prefix:"__" key then accum
      else
        Printf.sprintf "%s; %s -> %s" accum key (Expr.Pp.str data))
    store start
  ^ " }"
