type t = {
  name   : string;
  params : string list;
  body   : Stmt.t;
}

let create_store (func : t) (vals : Val.t list) : Store.t =
  let varvals = List.combine func.params vals in
  Store.create varvals


let  print_list (lis:string list) : string =
  String.concat ", "  lis


let create (name : string) (params : string list) (body : Stmt.t) : t = { name; params; body }

let get_name (func : t) : string = func.name

let get_params (func : t) : string list = func.params

let get_body (func : t) : Stmt.t = func.body

let str (func : t) : string =
  "function "
  ^ func.name
  ^ " ("
  ^ print_list func.params
  ^ ") { "
  ^ Stmt.str func.body
  ^ " }"

let to_json (func : t) : string =
  Printf.sprintf "{\"type\" : \"function\", \"name\" : \"%s\", \"params\" : [ %s ], \"body\" :  %s }" (func.name) (String.concat ", " (List.map (fun str -> Printf.sprintf "\"%s\"" str) func.params)) (Stmt.to_json func.body)