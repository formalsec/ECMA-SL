open Source

type t = t' Source.phrase

and t' =
  { name : Id.t
  ; params : Id.t list
  ; body : EStmt.t
  }

let default () : t =
  { name = Id.default (); params = []; body = EStmt.default () } @> no_region

let create (name : Id.t) (params : Id.t list) (body : EStmt.t) : t' =
  { name; params; body }

let name (m : t) : Id.t = m.it.name
let name' (m : t) : string = m.it.name.it
let params (m : t) : Id.t list = m.it.params
let params' (m : t) : string list = List.map (fun param -> param.it) m.it.params
let body (m : t) : EStmt.t = m.it.body

let pp_signature (fmt : Fmt.t) (m : t) : unit =
  let open Fmt in
  let { name; params; _ } = m.it in
  fprintf fmt "macro %a(%a)" Id.pp name (pp_lst ", " Id.pp) params

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
    match find_macro_f mn.it with
    | None -> Eslerr.(compile (UnknownMacro mn.it))
    | Some m ->
      let subst =
        try List.combine (params' m) es |> List.to_seq |> Hashtbl.of_seq
        with _ ->
          Eslerr.(compile (BadNArgs (List.length (params m), List.length es)))
      in
      EStmt.map ~emapper:(EExpr.Mapper.var subst) EStmt.Mapper.id (body m) )
  | _ -> s
