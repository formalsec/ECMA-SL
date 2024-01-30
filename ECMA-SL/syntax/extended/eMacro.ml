open Source

type t = t' Source.phrase

and t' =
  { name : Id.t
  ; params : Id.t list
  ; body : EStmt.t
  }

let default () : t =
  ?@{ name = Id.default (); params = []; body = EStmt.default () }

let create (name : Id.t) (params : Id.t list) (body : EStmt.t) : t' =
  { name; params; body }

let name (m : t) : Id.t = m.it.name
let name' (m : t) : Id.t' = m.it.name.it
let params (m : t) : Id.t list = m.it.params
let params' (m : t) : Id.t' list = List.map (fun px -> px.it) m.it.params
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
  Fmt.asprintf "%a" (if simple then pp_simple else pp) f

let mapper (find_macro_f : Id.t' -> t option) : EStmt.t -> EStmt.t =
 fun s ->
  match s.it with
  | EStmt.MacroApply (mn, es) -> (
    match find_macro_f mn.it with
    | None -> Eslerr.(compile ~src:(ErrSrc.at mn) (UnknownMacro mn.it))
    | Some m ->
      let subst =
        try List.combine (params' m) es |> List.to_seq |> Hashtbl.of_seq
        with _ ->
          let (npxs, nargs) = (List.length (params m), List.length es) in
          Eslerr.(compile ~src:(ErrSrc.at s) (BadNArgs (npxs, nargs)))
      in
      EStmt.map ~emapper:(EExpr.Mapper.var subst) EStmt.Mapper.id (body m) )
  | _ -> s
