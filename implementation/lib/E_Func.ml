type t = {
  name   : string;
  params : string list;
  body   : E_Stmt.t;
}

let create_store (func : t) (vals : E_Val.t list) : E_Store.t =
  let varvals = List.combine func.params vals in
  E_Store.create varvals

let create (name : string) (params : string list) (body : E_Stmt.t) : t = { name; params; body }

let get_name (func : t) : string = func.name
let get_params (func : t) : string list = func.params
let get_body (func : t) : E_Stmt.t = func.body

let str (func : t) : string =
  "function "
  ^ func.name
  ^ " ("
  ^ List.fold_left (fun acc ele -> (if acc <> "" then acc ^ ", " else acc) ^ ele) "" func.params
  ^ ") { "
  ^ E_Stmt.str func.body
  ^ " }"
