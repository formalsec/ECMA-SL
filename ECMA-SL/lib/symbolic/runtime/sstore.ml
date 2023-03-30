open Core
module Symbolic_store = Map.Make (String)

type bind = String.t
type t = Expr.t Symbolic_store.t

let create (values : (bind * Expr.t) list) : t =
  List.fold_left values ~init:Symbolic_store.empty ~f:(fun sto (key, data) ->
      Symbolic_store.add_exn sto ~key ~data)

let mem (store : t) (x : bind) : bool = Symbolic_store.mem store x

let add_exn (store : t) (key : bind) (data : Expr.t) : t =
  Symbolic_store.add_exn store ~key ~data

let find (store : t) (x : bind) : Expr.t option = Symbolic_store.find store x

let to_string (store : t) : string =
  Symbolic_store.fold store ~init:"{ " ~f:(fun ~key ~data accum ->
      (if String.equal accum "{ " then accum ^ ", " else accum)
      ^ Printf.sprintf "%s: %s" key (Expr.str data))
  ^ " }"
