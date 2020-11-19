type t = {
  name   : string;
  params : string list;
  body   : E_Stmt.t;
}

let create_store (func : t) (vals : Val.t list) : E_Store.t =
  let varvals = List.combine func.params vals in
  E_Store.create varvals

let create (name : string) (params : string list) (body : E_Stmt.t) : t = { name; params; body }

let get_name (func : t) : string = func.name
let get_params (func : t) : string list = func.params
let get_body (func : t) : E_Stmt.t = func.body

let print_list (lis : string list) : string = String.concat ", "  lis

let str (func : t) : string =
  "macro "
  ^ func.name
  ^ " ("
  ^ print_list func.params
  ^ ") "
  ^ E_Stmt.str func.body

let apply_macros_stmt (get_macro : string -> t option) (s : E_Stmt.t) : E_Stmt.t = 
  let mapper s = 
    match (s:E_Stmt.t) with 
    | MacroApply (m, es) -> 
      let macro = get_macro m in 
      (match macro with 
      | None -> raise (Failure ("Unknown macro "^m))
      | Some macro -> 
        if ((List.length es) <> (List.length macro.params)) then (
          raise (Failure ("Wrong Number of parameters given to: "^m))
        ) else (
          let subst = E_Expr.make_subst (List.combine macro.params es) in
          E_Stmt.subst subst macro.body 
        ))
    | _ -> s in 
  E_Stmt.map mapper s 