open EslBase
open Source

type t = t' Source.t

and t' =
  | Skip
  | Merge
  | Debug of t
  | Block of t list
  | Print of Expr.t
  | Return of Expr.t
  | Assign of Id.t * Expr.t
  | AssignCall of Id.t * Expr.t * Expr.t list
  | AssignECall of Id.t * Id.t * Expr.t list
  | AssignNewObj of Id.t
  | AssignObjToList of Id.t * Expr.t
  | AssignObjFields of Id.t * Expr.t
  | AssignInObjCheck of Id.t * Expr.t * Expr.t
  | FieldLookup of Id.t * Expr.t * Expr.t
  | FieldAssign of Expr.t * Expr.t * Expr.t
  | FieldDelete of Expr.t * Expr.t
  | If of Expr.t * t * t option
  | While of Expr.t * t
  | Switch of Expr.t * (Value.t, t) Hashtbl.t * t option
  | Fail of Expr.t
  | Assert of Expr.t

let default () : t = Skip @> none [@@inline]

let rec pp (ppf : Fmt.t) (stmt : t) : unit =
  let pp_vs pp_v ppf es = Fmt.(pp_lst !>", " pp_v) ppf es in
  let pp_indent pp_v ppf = Fmt.fmt ppf "@\n@[<v 2>  %a@]@\n" pp_v in
  match stmt.it with
  | Skip -> ()
  | Merge -> ()
  | Debug s -> Fmt.fmt ppf "# %a" pp s
  | Block ss -> Fmt.fmt ppf "{%a}" (pp_indent Fmt.(pp_lst !>";@\n" pp)) ss
  | Print e -> Fmt.fmt ppf "print %a" Expr.pp e
  | Return e ->
    if Expr.isvoid e then Fmt.pp_str ppf "return"
    else Fmt.fmt ppf "return %a" Expr.pp e
  | Fail e -> Fmt.fmt ppf "fail %a" Expr.pp e
  | Assert e -> Fmt.fmt ppf "assert %a" Expr.pp e
  | Assign (x, e) -> Fmt.fmt ppf "%a := %a" Id.pp x Expr.pp e
  | AssignCall (x, fe, es) ->
    Fmt.fmt ppf "%a := %a(%a)" Id.pp x Expr.pp fe (pp_vs Expr.pp) es
  | AssignECall (x, fn, es) ->
    Fmt.fmt ppf "%a := extern %a(%a)" Id.pp x Id.pp fn (pp_vs Expr.pp) es
  | AssignNewObj x -> Fmt.fmt ppf "%a := {}" Id.pp x
  | AssignObjToList (x, e) ->
    Fmt.fmt ppf "%a := obj_to_list %a" Id.pp x Expr.pp e
  | AssignObjFields (x, e) ->
    Fmt.fmt ppf "%a := obj_fields %a" Id.pp x Expr.pp e
  | AssignInObjCheck (x, e1, e2) ->
    Fmt.fmt ppf "%a := %a in_obj %a" Id.pp x Expr.pp e1 Expr.pp e2
  | FieldLookup (x, oe, fe) ->
    Fmt.fmt ppf "%a := %a[%a]" Id.pp x Expr.pp oe Expr.pp fe
  | FieldAssign (oe, fe, e) ->
    Fmt.fmt ppf "%a[%a] := %a" Expr.pp oe Expr.pp fe Expr.pp e
  | FieldDelete (oe, fe) -> Fmt.fmt ppf "delete %a[%a]" Expr.pp oe Expr.pp fe
  | If (e, s1, s2) ->
    let pp_else ppf s = Fmt.fmt ppf " else %a" pp s in
    Fmt.fmt ppf "if (%a) %a%a" Expr.pp e pp s1 (Fmt.pp_opt pp_else) s2
  | While (e, s) -> Fmt.fmt ppf "while (%a) %a" Expr.pp e pp s
  | Switch (e, css, dflt) ->
    let pp_dflt_cs ppf s = Fmt.fmt ppf "@\ndefault: %a" pp s in
    let pp_dflt ppf s = (Fmt.pp_opt pp_dflt_cs) ppf s in
    let pp_cs ppf (v, s) = Fmt.fmt ppf "case %a: %a" Value.pp v pp s in
    let pp_css ppf css = Fmt.(pp_hashtbl !>"@\n" pp_cs) ppf css in
    let pp ppf (css, dflt) = Fmt.fmt ppf "%a%a" pp_css css pp_dflt dflt in
    Fmt.fmt ppf "switch (%a) {%a}" Expr.pp e (pp_indent pp) (css, dflt)

let pp_simple (ppf : Fmt.t) (stmt : t) : unit =
  match stmt.it with
  | Block _ -> Fmt.fmt ppf "{ ... }"
  | If (e, _, _) -> Fmt.fmt ppf "if (%a) { ..." Expr.pp e
  | While (e, _) -> Fmt.fmt ppf "while (%a) { ..." Expr.pp e
  | Switch (e, _, _) -> Fmt.fmt ppf "switch (%a) { ..." Expr.pp e
  | _ -> pp ppf stmt

let str (stmt : t) : string = Fmt.str "%a" pp stmt [@@inline]
