open Source

type t = t' Source.phrase

and t' =
  { metadata : E_Func_Metadata.t option
  ; name : string
  ; params_t : (string * EType.t option) list
  ; return_t : EType.t option
  ; (* list_param: list_param_t option; *)
    body : EStmt.t
  }

(* type list_param_t = string list * string list * string option *)
(* required, optionals, summary_list *)
(* target -> list_param_t_target *)

let create (metadata : E_Func_Metadata.t option) (name : string)
  (params_t : (string * EType.t option) list) (return_t : EType.t option)
  (body : EStmt.t) : t' =
  { metadata; name; params_t; return_t; body }

let default () : t' =
  { metadata = None
  ; name = ""
  ; params_t = []
  ; return_t = None
  ; body = EStmt.default ()
  }

let get_name (func : t) : string = func.it.name
let get_params_t (func : t) : (string * EType.t option) list = func.it.params_t
let get_return_t (func : t) : EType.t option = func.it.return_t
let get_body (func : t) : EStmt.t = func.it.body
let get_metadata (func : t) : E_Func_Metadata.t option = func.it.metadata
let print_list (lis : string list) : string = String.concat ", " lis

let get_params (func : t) : string list =
  List.map (fun p -> fst p) func.it.params_t

let get_tparams (func : t) : EType.t list =
  List.map
    (fun (_, t) -> Option.value ~default:EType.AnyType t)
    func.it.params_t

let create_store (func : t) (vals : Val.t list) : Val.t Store.t =
  let params = get_params func in
  let varvals = List.combine params vals in
  Store.create varvals

let str (func : t) : string =
  let param_str (p : string) (t : EType.t option) : string =
    match t with None -> p | Some t' -> p ^ ": " ^ EType.str t'
  in
  let return_str (return_t : EType.t option) : string =
    match return_t with
    | None -> ""
    | Some return_t' -> ": " ^ EType.str return_t'
  in
  let params_str = List.map (fun (p, t) -> param_str p t) func.it.params_t in
  "function "
  ^ func.it.name
  ^ " ("
  ^ print_list params_str
  ^ ") "
  ^ return_str func.it.return_t
  ^ EStmt.str func.it.body

let apply_macros (f : t) (macros : string -> EMacro.t option) : t =
  let new_body = EMacro.apply_macros_stmt macros f.it.body in
  { f with it = { f.it with body = new_body } }

let lambdas (f : t) : (string * string list * string list * EStmt.t) list =
  EStmt.lambdas f.it.body
