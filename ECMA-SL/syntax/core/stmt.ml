open Source

type t = t' Source.phrase

and t' =
  | Skip
  | Merge
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
  | Abort of Expr.t

let rec pp (fmt : Fmt.formatter) (s : t) : unit =
  let open Fmt in
  match s.it with
  | Skip -> fprintf fmt "skip"
  | Merge -> fprintf fmt "merge"
  | Block stmts -> fprintf fmt "%a" (pp_lst ";\n" pp) stmts
  | Print e -> fprintf fmt "print %a" Expr.pp e
  | Return e -> fprintf fmt "return %a" Expr.pp e
  | Assign (x, e) -> fprintf fmt "%s := %a" x Expr.pp e
  | AssignCall (x, fe, es) ->
    fprintf fmt "%s := %a(%a)" x Expr.pp fe (pp_lst ", " Expr.pp) es
  | AssignECall (x, fn, es) ->
    fprintf fmt "%s := extern %s(%a)" x fn (pp_lst ", " Expr.pp) es
  | AssignNewObj x -> fprintf fmt "%s := { }" x
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
    let pp_else fmt v = fprintf fmt " else {\n%a\n}" pp v in
    fprintf fmt "if (%a) {\n%a\n}%a" Expr.pp e pp s1 (pp_opt pp_else) s2
  | While (e, s') -> fprintf fmt "while (%a) {\n%a\n}" Expr.pp e pp s'
  | Fail e -> fprintf fmt "fail %a" Expr.pp e
  | Assert e -> fprintf fmt "assert (%a)" Expr.pp e
  | Abort e -> fprintf fmt "abort %a" Expr.pp e

let str (s : t) : string = Fmt.asprintf "%a" pp s

module Pp = struct
  let to_string (stmt : t) pp : string =
    let str = pp in
    let concat es = String.concat ", " (List.map str es) in
    match stmt.it with
    | Skip -> "skip"
    | Merge -> "merge"
    | Print e -> Format.sprintf "print %s" (str e)
    | Fail e -> Format.sprintf "fail %s" (str e)
    | Assign (lval, rval) -> Format.sprintf "%s := %s" lval (str rval)
    | If (cond, _, _) -> Format.sprintf "if (%s) { ... }" (str cond)
    | Block _ -> "block { ... }"
    | While (cond, _) -> Format.sprintf "while (%s) { ... }" (str cond)
    | Return exp -> Format.sprintf "return %s" (str exp)
    | FieldAssign (e_o, f, e_v) ->
      Format.sprintf "%s[%s] := %s" (str e_o) (str f) (str e_v)
    | FieldDelete (e, f) -> Format.sprintf "delete %s[%s]" (str e) (str f)
    | AssignCall (va, st, e_lst) ->
      Format.sprintf "%s := %s(%s)" va (str st) (concat e_lst)
    | AssignECall (x, f, es) ->
      Format.sprintf "%s := extern %s(%s)" x f (concat es)
    | AssignNewObj va -> Format.sprintf "%s := {}" va
    | FieldLookup (va, eo, p) ->
      Format.sprintf "%s := %s[%s]" va (str eo) (str p)
    | AssignInObjCheck (st, e1, e2) ->
      Format.sprintf "%s := %s in_obj %s" st (str e1) (str e2)
    | AssignObjToList (st, e) ->
      Format.sprintf "%s := obj_to_list %s" st (str e)
    | AssignObjFields (st, e) -> Format.sprintf "%s := obj_fields %s" st (str e)
    | Assert e -> Format.sprintf "assert (%s)" (str e)
    | Abort e -> Format.sprintf "se_abort (%s)" (str e)
end
