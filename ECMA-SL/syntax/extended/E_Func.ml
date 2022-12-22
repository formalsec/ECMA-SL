type t = {
  metadata : E_Func_Metadata.t option;
  name : string;
  params : string list;
  (* list_param: list_param_t option; *)
  body : E_Stmt.t;
}
(* type list_param_t = string list * string list * string option *)
(* required, optionals, summary_list *)
(* target -> list_param_t_target *)

let create_store (func : t) (vals : Val.t list) : E_Store.t =
  let varvals = List.combine func.params vals in
  E_Store.create varvals

let create (metadata : E_Func_Metadata.t option) (name : string)
    (params : string list) (body : E_Stmt.t) : t =
  { metadata; name; params; body }

let get_name (func : t) : string = func.name
let get_params (func : t) : string list = func.params
let get_body (func : t) : E_Stmt.t = func.body
let get_metadata (func : t) : E_Func_Metadata.t option = func.metadata
let print_list (lis : string list) : string = String.concat ", " lis

let str (func : t) : string =
  "function " ^ func.name ^ " (" ^ print_list func.params ^ ") "
  ^ E_Stmt.str func.body

let apply_macros (f : t) (macros : string -> E_Macro.t option) : t =
  let new_body = E_Macro.apply_macros_stmt macros f.body in
  { f with body = new_body }

let lambdas (f : t) : (string * string list * string list * E_Stmt.t) list =
  E_Stmt.lambdas f.body
