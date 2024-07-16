open EslBase
open EslSyntax
open EslSyntax.Source

module Stmt = struct
  open Stmt

  let parse_return (expr : Expr.t option) : Expr.t =
    Option.value ~default:(Expr.Val (App (`Op "void", [])) @> none) expr

  let parse_switch_cases (css : (Expr.t * t) list) : (Value.t, t) Hashtbl.t =
    let val_of_expr e =
      match e.it with
      | Expr.Val v -> (v, e.at)
      | _ -> Log.fail "expecting a value expression, but got %a" Expr.pp e
    in
    let check_dups css (e, s) =
      let (v, at) = val_of_expr e in
      if not (Hashtbl.mem css v) then Hashtbl.replace css v s
      else Compile_error.(throw ~src:(ErrSrc.at at) (DuplicatedSwitchCase v))
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
