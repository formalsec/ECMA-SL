open EslSyntax
open EslSyntax.Source

module Stmt = struct
  open EslSyntax.Stmt

  let parse_switch_cases (switch_cases : (Val.t Source.phrase * t) list) :
    (Val.t, t) Hashtbl.t =
    let set_case css (v, s) =
      if not (Hashtbl.mem css v.it) then Hashtbl.replace css v.it s
      else Compile_error.(throw ~src:(ErrSrc.at v) (DuplicatedSwitchCase v.it))
    in
    let css = Hashtbl.create (List.length switch_cases) in
    List.iter (set_case css) switch_cases;
    css
end
