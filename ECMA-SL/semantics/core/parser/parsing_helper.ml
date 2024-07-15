open EslSyntax
open EslSyntax.Source

module Stmt = struct
  open Stmt

  let parse_return (e : Expr.t option) : Expr.t =
    let default = Expr.Val (Value.App (`Op "void", [])) @> none in
    Option.value ~default e

  let parse_switch_cases (css : (Value.t Source.t * t) list) :
    (Value.t, t) Hashtbl.t =
    let check_dups css (v, s) =
      if not (Hashtbl.mem css v.it) then Hashtbl.replace css v.it s
      else Compile_error.(throw ~src:(ErrSrc.from v) (DuplicatedSwitchCase v.it))
    in
    let parsed_css = Hashtbl.create (List.length css) in
    List.iter (check_dups parsed_css) css;
    parsed_css
end

module Func = struct
  let parse_params (pxs : Id.t list) : Id.t list =
    let check_dups checked px =
      if not (Hashtbl.mem checked px.it) then Hashtbl.replace checked px.it ()
      else Compile_error.(throw ~src:(ErrSrc.from px) (DuplicatedParam px))
    in
    List.iter (check_dups (Hashtbl.create (List.length pxs))) pxs;
    pxs
end
