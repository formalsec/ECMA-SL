type t = t' Source.phrase

and t' =
  { name : string
  ; params : string list
  ; body : EStmt.t
  }

let create (name : string) (params : string list) (body : EStmt.t) : t' =
  { name; params; body }

let name (m : t) : string = m.it.name
let params (m : t) : string list = m.it.params
let body (m : t) : EStmt.t = m.it.body

let pp_signature (fmt : Fmt.t) (m : t) : unit =
  let open Fmt in
  let { name; params; _ } = m.it in
  fprintf fmt "macro %s(%a)" name (pp_lst ", " pp_str) params

let pp (fmt : Fmt.t) (m : t) : unit =
  Fmt.fprintf fmt "%a %a" pp_signature m EStmt.pp m.it.body

let pp_simple (fmt : Fmt.t) (m : t) : unit =
  Fmt.fprintf fmt "%a {..." pp_signature m

let str ?(simple : bool = false) (f : t) : string =
  if simple then Fmt.asprintf "%a" pp_simple f else Fmt.asprintf "%a" pp f

(* FIXME: Requires cleaning below *)
let apply_macros_stmt (get_macro : string -> t option) (s : EStmt.t) : EStmt.t =
  let mapper s =
    match s.Source.it with
    | EStmt.MacroApply (m, es) -> (
      let macro = get_macro m in
      match macro with
      | None -> raise (Failure ("Unknown macro " ^ m))
      | Some { it = macro; _ } ->
        if List.length es <> List.length macro.params then
          raise (Failure ("Wrong Number of parameters given to: " ^ m))
        else
          let subst = EExpr.make_subst (List.combine macro.params es) in
          EStmt.subst subst macro.body )
    | _ -> s
  in
  EStmt.map mapper s
