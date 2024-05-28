open EslBase
open Source

type t = t' Source.phrase

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
  | Switch of Expr.t * (Val.t, t) Hashtbl.t * t option
  | Fail of Expr.t
  | Assert of Expr.t

let default () : t = Skip @> no_region

let rec pp (ppf : Fmt.t) (s : t) : unit =
  let open Fmt in
  let pp_return ppf e =
    if Expr.isvoid e then () else fprintf ppf " %a" Expr.pp e
  in
  match s.it with
  | Skip -> fprintf ppf "skip"
  | Merge -> fprintf ppf "merge"
  | Debug s' -> fprintf ppf "# %a" pp s'
  | Block ss ->
    fprintf ppf "{@\n@[<v 2>  %a@]@\n}"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@\n") pp)
      ss
  | Print e -> fprintf ppf "print %a" Expr.pp e
  | Return e -> fprintf ppf "return%a" pp_return e
  | Assign (x, e) -> fprintf ppf "%a := %a" Id.pp x Expr.pp e
  | AssignCall (x, fe, es) ->
    fprintf ppf "%a := %a(%a)" Id.pp x Expr.pp fe (pp_lst ", " Expr.pp) es
  | AssignECall (x, fn, es) ->
    fprintf ppf "%a := extern %a(%a)" Id.pp x Id.pp fn (pp_lst ", " Expr.pp) es
  | AssignNewObj x -> fprintf ppf "%a := {}" Id.pp x
  | AssignObjToList (x, e) ->
    fprintf ppf "%a := obj_to_list %a" Id.pp x Expr.pp e
  | AssignObjFields (x, e) ->
    fprintf ppf "%a := obj_fields %a" Id.pp x Expr.pp e
  | AssignInObjCheck (x, e1, e2) ->
    fprintf ppf "%a := %a in_obj %a" Id.pp x Expr.pp e1 Expr.pp e2
  | FieldLookup (x, oe, fe) ->
    fprintf ppf "%a := %a[%a]" Id.pp x Expr.pp oe Expr.pp fe
  | FieldAssign (oe, fe, e) ->
    fprintf ppf "%a[%a] := %a" Expr.pp oe Expr.pp fe Expr.pp e
  | FieldDelete (oe, fe) -> fprintf ppf "delete %a[%a]" Expr.pp oe Expr.pp fe
  | If (e, s1, s2) ->
    let pp_else ppf v = fprintf ppf " else %a" pp v in
    fprintf ppf "if (%a) %a%a" Expr.pp e pp s1 (pp_opt pp_else) s2
  | While (e, s') -> fprintf ppf "while (%a) %a" Expr.pp e pp s'
  | Switch (e, css, dflt) ->
    let pp_case ppf (v, s) = fprintf ppf "\ncase %a: %a" Val.pp v pp s in
    let pp_default ppf s = fprintf ppf "\nsdefault: %a" pp s in
    fprintf ppf "switch (%a) {%a%a\n}" Expr.pp e (pp_hashtbl "" pp_case) css
      (pp_opt pp_default) dflt
  | Fail e -> fprintf ppf "fail %a" Expr.pp e
  | Assert e -> fprintf ppf "assert %a" Expr.pp e

let pp_simple (ppf : Fmt.t) (s : t) : unit =
  let open Fmt in
  match s.it with
  | Block _ -> fprintf ppf "block { ... }"
  | If (e, _, _) -> fprintf ppf "if (%a) { ..." Expr.pp e
  | While (e, _) -> fprintf ppf "while (%a) { ..." Expr.pp e
  | _ -> pp ppf s

let str ?(simple : bool = false) (s : t) : string =
  Fmt.asprintf "%a" (if simple then pp_simple else pp) s
