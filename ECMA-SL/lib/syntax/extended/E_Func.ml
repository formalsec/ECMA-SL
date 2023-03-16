open Source

type t = t' Source.phrase

and t' = {
  metadata : E_Func_Metadata.t option;
  name : string;
  params_t : (string * E_Type.t option) list;
  return_t : E_Type.t option;
  (* list_param: list_param_t option; *)
  body : E_Stmt.t;
}

(* type list_param_t = string list * string list * string option *)
(* required, optionals, summary_list *)
(* target -> list_param_t_target *)

let create (metadata : E_Func_Metadata.t option) (name : string)
    (params_t : (string * E_Type.t option) list) (return_t : E_Type.t option)
    (body : E_Stmt.t) : t' =
  { metadata; name; params_t; return_t; body }

let get_name (func : t) : string = func.it.name
let get_params_t (func : t) : (string * E_Type.t option) list = func.it.params_t
let get_return_t (func : t) : E_Type.t option = func.it.return_t
let get_body (func : t) : E_Stmt.t = func.it.body
let get_metadata (func : t) : E_Func_Metadata.t option = func.it.metadata
let print_list (lis : string list) : string = String.concat ", " lis

let get_params (func : t) : string list =
  List.map (fun p -> fst p) func.it.params_t

let create_store (func : t) (vals : Val.t list) : E_Store.t =
  let params = get_params func in
  let varvals = List.combine params vals in
  E_Store.create varvals

let str (func : t) : string =
  let param_str (p : string) (t : E_Type.t option) : string =
    match t with None -> p | Some t' -> p ^ ": " ^ E_Type.str t'
  in
  let return_str (return_t : E_Type.t option) : string =
    match return_t with
    | None -> ""
    | Some return_t' -> ": " ^ E_Type.str return_t'
  in
  let params_str = List.map (fun (p, t) -> param_str p t) func.it.params_t in
  "function " ^ func.it.name ^ " (" ^ print_list params_str ^ ") "
  ^ return_str func.it.return_t
  ^ E_Stmt.str func.it.body

let apply_macros (f : t) (macros : string -> E_Macro.t option) : t =
  let new_body = E_Macro.apply_macros_stmt macros f.it.body in
  { f with it = { f.it with body = new_body } }

let lambdas (f : t) : (string * string list * string list * E_Stmt.t) list =
  E_Stmt.lambdas f.it.body
