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
  | Throw of string
  | Fail of Expr.t
  | Assert of Expr.t
  | Abort of Expr.t

let rec str ?(expr_printer : Expr.t -> string = Expr.str) (stmt : t) : string =
  let _str_e e = expr_printer e in
  let _str_es es = String.concat ", " (List.map _str_e es) in
  match stmt.it with
  | Skip -> ""
  | Merge -> ""
  | Block stmts -> List.map (str ~expr_printer) stmts |> String.concat ";\n"
  | Print e -> Printf.sprintf "print %s" (_str_e e)
  | Return e -> Printf.sprintf "return %s" (_str_e e)
  | Assign (x, e) -> Printf.sprintf "%s := %s" x (_str_e e)
  | AssignCall (x, fe, es) ->
    Printf.sprintf "%s := %s(%s)" x (_str_e fe) (_str_es es)
  | AssignECall (x, fn, es) ->
    Printf.sprintf "%s := extern %s(%s)" x fn (_str_es es)
  | AssignNewObj x -> Printf.sprintf "%s := { }" x
  | AssignObjToList (x, e) -> Printf.sprintf "%s := obj_to_list %s" x (_str_e e)
  | AssignObjFields (x, e) -> Printf.sprintf "%s := obj_fields %s" x (_str_e e)
  | AssignInObjCheck (x, e1, e2) ->
    Printf.sprintf "%s := %s in_obj %s" x (_str_e e1) (_str_e e2)
  | FieldLookup (x, oe, fe) ->
    Printf.sprintf "%s := %s[%s]" x (_str_e oe) (_str_e fe)
  | FieldAssign (oe, fe, e) ->
    Printf.sprintf "%s[%s] := %s" (_str_e oe) (_str_e fe) (_str_e e)
  | FieldDelete (oe, fe) ->
    Printf.sprintf "delete %s[%s]" (_str_e oe) (_str_e fe)
  | If (e, s1, s2) ->
    let else_str = function
      | Some s -> Printf.sprintf " else {\n%s\n}" (str s)
      | None -> ""
    in
    Printf.sprintf "if (%s) {\n%s\n}%s" (_str_e e) (str s1) (else_str s2)
  | While (e, s) -> Printf.sprintf "while (%s) {\n%s\n}" (_str_e e) (str s)
  | Throw s -> Printf.sprintf "throw \"%s\"" s
  | Fail e -> Printf.sprintf "fail %s" (_str_e e)
  | Assert e -> Printf.sprintf "assert (%s)" (_str_e e)
  | Abort e -> Printf.sprintf "abort %s" (_str_e e)

let rec to_json (stmt : t) : string =
  let to_json_exprs exprs = List.map Expr.to_json exprs |> String.concat ", "
  and to_json_stmts stmts = List.map to_json stmts |> String.concat ", " in
  match stmt.it with
  | Skip -> Printf.sprintf "{ \"type\" : \"skip\" }"
  | Merge -> Printf.sprintf "{ \"type\" : \"merge\" }"
  | Block stmts ->
    Printf.sprintf "{ \"type\" : \"block\", \"value\" : [ %s ] }"
      (to_json_stmts stmts)
  | Print e ->
    Printf.sprintf "{ \"type\" : \"print\", \"expr\" : %s }" (Expr.to_json e)
  | Return e ->
    Printf.sprintf "{ \"type\" : \"return\", \"expr\" : %s }" (Expr.to_json e)
  | Assign (x, e) ->
    Printf.sprintf "{ \"type\" : \"assign\", \"lhs\" : \"%s\", \"rhs\" : %s }" x
      (Expr.to_json e)
  | AssignCall (x, fe, es) ->
    Printf.sprintf
      "{ \"type\" : \"assigncall\", \"lhs\" : \"%s\", \"func\" : %s, \"args\" \
       : [ %s ] }"
      x (Expr.to_json fe) (to_json_exprs es)
  | AssignECall (x, fn, es) ->
    Printf.sprintf
      "{ \"type\" : \"assignecall\", \"lhs\" : \"%s\", \"func\" : %s, \"args\" \
       : [ %s ] }"
      x fn (to_json_exprs es)
  | AssignNewObj x ->
    Printf.sprintf "{ \"type\" : \"assignnewobject\", \"lhs\" : \"%s\" }" x
  | AssignObjToList (x, e) ->
    Printf.sprintf
      "{ \"type\" : \"assigniobjtolist\", \"lhs\" : \"%s\", \"obj\" : %s }" x
      (Expr.to_json e)
  | AssignObjFields (x, e) ->
    Printf.sprintf
      "{ \"type\" : \"assignobjfields\", \"lhs\" : \"%s\", \"obj\" : %s }" x
      (Expr.to_json e)
  | AssignInObjCheck (x, e1, e2) ->
    Printf.sprintf
      "{ \"type\" : \"assigninobjcheck\", \"lhs\" : \"%s\", \"field\" : %s, \
       \"obj\" : %s }"
      x (Expr.to_json e1) (Expr.to_json e2)
  | FieldLookup (x, oe, fe) ->
    Printf.sprintf
      "{ \"type\" : \"fieldlookup\", \"lhs\" : \"%s\", \"obj\" : %s, \"field\" \
       : %s }"
      x (Expr.to_json oe) (Expr.to_json fe)
  | FieldAssign (oe, fe, e) ->
    Printf.sprintf
      "{ \"type\" : \"fieldassign\", \"obj\" : %s, \"field\" : %s, \"value\" : \
       %s }"
      (Expr.to_json oe) (Expr.to_json fe) (Expr.to_json e)
  | FieldDelete (oe, fe) ->
    Printf.sprintf
      "{ \"type\" : \"fielddelete\", \"obj\" : %s, \"field\" : %s }"
      (Expr.to_json oe) (Expr.to_json fe)
  | If (e, s1, None) ->
    Printf.sprintf "{ \"type\" : \"condition\", \"expr\" : %s, \"then\" : %s }"
      (Expr.to_json e) (to_json s1)
  | If (e, s1, Some s2) ->
    Printf.sprintf
      "{ \"type\" : \"condition\", \"expr\" : %s, \"then\" : %s, \"else\" : %s \
       }"
      (Expr.to_json e) (to_json s1) (to_json s2)
  | While (e, s) ->
    Printf.sprintf "{ \"type\" : \"loop\", \"expr\" : %s, \"do\" : %s }"
      (Expr.to_json e) (to_json s)
  | Throw exn ->
    Printf.sprintf "{ \"type\" : \"exception\", \"value\" : \"%s\" }" exn
  | Assert e ->
    Printf.sprintf "{ \"type\" : \"assert\", \"expr\" : %s }" (Expr.to_json e)
  | Fail e ->
    Printf.sprintf "{ \"type\" : \"fail\", \"expr\" : %s }" (Expr.to_json e)
  | Abort e ->
    Printf.sprintf "{ \"type\" : \"abort\", \"expr\" : %s }" (Expr.to_json e)

let is_basic_stmt (s : t) : bool =
  match s.it with
  | If _ | While _ | Block _ -> false
  | _ -> true

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
    | Throw st -> Format.sprintf "throw \"%s\"" st
    | Assert e -> Format.sprintf "assert (%s)" (str e)
    | Abort e -> Format.sprintf "se_abort (%s)" (str e)
end
