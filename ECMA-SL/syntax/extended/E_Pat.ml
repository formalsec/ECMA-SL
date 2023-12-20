open Source

type t = t' Source.phrase

and t' =
  | ObjPat of (string * E_Pat_v.t) list * E_Pat_Metadata.t option
  | DefaultPat

let str (pat : t) : string =
  match pat.it with
  | ObjPat (pn_pats, _) ->
    "{ "
    ^ String.concat ", "
        (List.map (fun (pn, pat) -> pn ^ ": " ^ E_Pat_v.str pat) pn_pats)
    ^ "}"
  | DefaultPat -> "default"
