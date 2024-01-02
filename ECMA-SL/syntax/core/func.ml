open Source

type t = t' Source.phrase

and t' =
  { name : string
  ; params : string list
  ; body : Stmt.t
  }

let name (func : t) : string = func.it.name
let params (func : t) : string list = func.it.params
let body (func : t) : Stmt.t = func.it.body

let create (name : string) (params : string list) (body : Stmt.t) : t' =
  { name; params; body }

let str (func : t) : string =
  let _params_str = String.concat ", " func.it.params in
  let _body_str = Stmt.str func.it.body in
  Printf.sprintf "function %s(%s) {\n%s\n}" func.it.name _params_str _body_str

let to_json (func : t) : string =
  let { name; params; body } = func.it in
  let _json_param_f param = Printf.sprintf "\"%s\"" param in
  let _params_json = List.map _json_param_f params |> String.concat ", " in
  let _body_json = Stmt.to_json body in
  Printf.sprintf
    "{ \"type\" : \"function\", \"name\" : \"%s\", \"params\" : [ %s ], \
     \"body\" : %s }"
    name _params_json _body_json
