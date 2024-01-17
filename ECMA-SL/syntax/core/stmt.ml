open Source

type t = t' Source.phrase

and t' =
  | Skip
  | Merge
  | Debug of t
  | Block of t list
  | Print of Expr.t
  | Return of Expr.t
  | Assign of string * Expr.t
  | AssignCall of string * Expr.t * Expr.t list
  | AssignECall of string * string * Expr.t list
  | AssignNewObj of string
  | AssignObjToList of string * Expr.t
  | AssignObjFields of string * Expr.t
  | AssignInObjCheck of string * Expr.t * Expr.t
  | FieldLookup of string * Expr.t * Expr.t
  | FieldAssign of Expr.t * Expr.t * Expr.t
  | FieldDelete of Expr.t * Expr.t
  | If of Expr.t * t * t option
  | While of Expr.t * t
  | Fail of Expr.t
  | Assert of Expr.t

let rec pp (fmt : Fmt.t) (s : t) : unit =
  let open Fmt in
  match s.it with
  | Skip -> fprintf fmt "skip"
  | Merge -> fprintf fmt "merge"
  | Debug s' -> fprintf fmt "# %a" pp s'
  | Block ss -> fprintf fmt "{\n%a\n}" (pp_lst ";\n" pp) ss
  | Print e -> fprintf fmt "print %a" Expr.pp e
  | Return e -> fprintf fmt "return %a" Expr.pp e
  | Assign (x, e) -> fprintf fmt "%s := %a" x Expr.pp e
  | AssignCall (x, fe, es) ->
    fprintf fmt "%s := %a(%a)" x Expr.pp fe (pp_lst ", " Expr.pp) es
  | AssignECall (x, fn, es) ->
    fprintf fmt "%s := extern %s(%a)" x fn (pp_lst ", " Expr.pp) es
  | AssignNewObj x -> fprintf fmt "%s := {}" x
  | AssignObjToList (x, e) -> fprintf fmt "%s := obj_to_list %a" x Expr.pp e
  | AssignObjFields (x, e) -> fprintf fmt "%s := obj_fields %a" x Expr.pp e
  | AssignInObjCheck (x, e1, e2) ->
    fprintf fmt "%s := %a in_obj %a" x Expr.pp e1 Expr.pp e2
  | FieldLookup (x, oe, fe) ->
    fprintf fmt "%s := %a[%a]" x Expr.pp oe Expr.pp fe
  | FieldAssign (oe, fe, e) ->
    fprintf fmt "%a[%a] := %a" Expr.pp oe Expr.pp fe Expr.pp e
  | FieldDelete (oe, fe) -> fprintf fmt "delete %a[%a]" Expr.pp oe Expr.pp fe
  | If (e, s1, s2) ->
    let pp_else fmt v = fprintf fmt " else %a" pp v in
    fprintf fmt "if (%a) %a%a" Expr.pp e pp s1 (pp_opt pp_else) s2
  | While (e, s') -> fprintf fmt "while (%a) %a" Expr.pp e pp s'
  | Fail e -> fprintf fmt "fail %a" Expr.pp e
  | Assert e -> fprintf fmt "assert %a" Expr.pp e

let pp_simple (fmt : Fmt.t) (s : t) : unit =
  let open Fmt in
  match s.it with
  | Block _ -> fprintf fmt "block { ... }"
  | If (e, _, _) -> fprintf fmt "if (%a) { ..." Expr.pp e
  | While (e, _) -> fprintf fmt "while (%a) { ..." Expr.pp e
  | _ -> pp fmt s

let str ?(simple : bool = false) (s : t) : string =
  if simple then Fmt.asprintf "%a" pp_simple s else Fmt.asprintf "%a" pp s
