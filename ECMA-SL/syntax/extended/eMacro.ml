open Source

type t = t' Source.phrase

and t' =
  { name : string
  ; params : string list
  ; body : EStmt.t
  }

let default () : t =
  { name = ""; params = []; body = EStmt.default () } @> no_region

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

let mapper (find_macro_f : string -> t option) : EStmt.t -> EStmt.t =
 fun s ->
  match s.it with
  | EStmt.MacroApply (mn, es) -> (
    match find_macro_f mn with
    | None -> Eslerr.(compile (UnknownMacro mn))
    | Some { it = m; _ } ->
      let subst =
        try List.combine m.params es |> List.to_seq |> Hashtbl.of_seq
        with _ ->
          let (nparams, nargs) = (List.length m.params, List.length es) in
          Eslerr.(compile (BadNArgs (nparams, nargs)))
      in
      EStmt.map ~emapper:(EExpr.Mapper.var subst) EStmt.Mapper.id m.body )
  | _ -> s
