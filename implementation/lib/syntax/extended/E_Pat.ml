type t =
  | ObjPat     of (string * E_Pat_v.t) list
  | DefaultPat


let str (pat : t) : string =
  match pat with
  | ObjPat pn_pats -> "{ " ^ String.concat ", " (List.map (fun (pn, pat) -> pn ^ ": " ^ E_Pat_v.str pat) pn_pats) ^ "}"
  | DefaultPat     -> "default"
