open Smtml
open EslSyntax
open EslSyntax.Source

module Stmt = struct
  open EslSyntax.Stmt

  let parse_switch_cases (switch_cases : (Value.t Source.phrase * t) list) :
    (Value.t, t) Hashtbl.t =
    let set_case css (v, s) =
      if not (Hashtbl.mem css v.it) then Hashtbl.replace css v.it s
      else Compile_error.(throw ~src:(ErrSrc.at v) (DuplicatedSwitchCase v.it))
    in
    let css = Hashtbl.create (List.length switch_cases) in
    List.iter (set_case css) switch_cases;
    css
end

module Prog = struct
  let parse_params (pxs : Id.t list) : Id.t list =
    let check_dups checked px =
      if not (Hashtbl.mem checked px.it) then Hashtbl.replace checked px.it ()
      else Compile_error.(throw ~src:(ErrSrc.at px) (DuplicatedParam px))
    in
    List.iter (check_dups (Hashtbl.create (List.length pxs))) pxs;
    pxs
end
