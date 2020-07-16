type t = {
  name   : string;
  params : string list;
  body   : Stmt.t list;
}

let create_store (func : t) (vals : Val.t list) : Store.t =
  let varvals = List.combine func.params vals in
  Store.create varvals


let  print_list (lis:string list) : string =
	String.concat ", "  lis


let create (name : string) (params : string list) (body : Stmt.t list) : t = { name; params; body }

let get_name (func : t) : string = func.name

let get_params (func : t) : string list = func.params

let get_body (func : t) : Stmt.t list = func.body

let str (func : t) : string =  "\nfunction " ^ func.name ^ " (" ^ (print_list func.params)^") {\n"^"Oi"^"\n}"
