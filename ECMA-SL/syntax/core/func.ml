type t =
  { name : string
  ; params : string list
  ; body : Stmt.t
  }

let name (func : t) : string = func.name
let params (func : t) : string list = func.params
let body (func : t) : Stmt.t = func.body

let create (name : string) (params : string list) (body : Stmt.t) : t =
  { name; params; body }

let create_store (func : t) (vals : Val.t list) : Val.t Store.t =
  let var_vals = List.combine func.params vals in
  Store.create var_vals

let print_list (lis : string list) : string = String.concat ", " lis

let str (func : t) : string =
  let _params_str = String.concat ", " func.params in
  let _body_str = Stmt.str func.body in
  Printf.sprintf "function %s(%s) {\n%s\n}" func.name _params_str _body_str

let to_json (func : t) : string =
  let _param_json_f param = Printf.sprintf "\"%s\"" param in
  let _params_json = List.map _param_json_f func.params |> String.concat ", " in
  let _body_json = Stmt.to_json func.body in
  Printf.sprintf
    "{ \"type\" : \"function\", \"name\" : \"%s\", \"params\" : [ %s ], \
     \"body\" : %s }"
    func.name _params_json _body_json
