open Core
open Source

type t = t' Source.phrase

and t' =
  | Skip
  | Merge
  | Print of Expr.t
  | Fail of Expr.t
  | Abort of Expr.t
  | Assert of Expr.t
  | Assign of string * Expr.t
  | If of Expr.t * t * t option
  | While of Expr.t * t
  | Return of Expr.t
  | AssignCall of string * Expr.t * Expr.t list
  | AssignECall of string * string * Expr.t list
  | AssignNewObj of string
  | AssignInObjCheck of string * Expr.t * Expr.t
  | AssignObjToList of string * Expr.t
  | AssignObjFields of string * Expr.t
  | Block of t list
  | FieldAssign of Expr.t * Expr.t * Expr.t
  | FieldDelete of Expr.t * Expr.t
  | FieldLookup of string * Expr.t * Expr.t
  | Exception of string
  | SymStmt of SymStmt.t

(*---------------Strings------------------*)

let is_basic_stmt (s : t) : bool =
  match s.it with If _ | While _ | Block _ -> false | _ -> true

let rec str ?(print_expr : (Expr.t -> string) option) (stmt : t) : string =
  let str_e = Option.value print_expr ~default:Expr.str in
  let str_es es = String.concat ~sep:", " (List.map ~f:str_e es) in
  match stmt.it with
  | Skip | Merge -> ""
  | Print e -> "print " ^ str_e e
  | Fail e -> "fail " ^ str_e e
  | Assign (v, exp) -> v ^ " := " ^ str_e exp
  | If (e, s1, s2) -> (
      let v = "if (" ^ str_e e ^ ") {\n" ^ str s1 ^ "\n}" in
      match s2 with None -> v | Some s -> v ^ " else {\n" ^ str s ^ "\n}")
  | Block block ->
      String.concat ~sep:";\n" (List.map ~f:(str ~print_expr:str_e) block)
  | While (exp, s) -> "while (" ^ str_e exp ^ ") { " ^ str s ^ " }"
  | Return exp -> "return " ^ str_e exp
  | FieldAssign (e_o, f, e_v) -> str_e e_o ^ "[" ^ str_e f ^ "] := " ^ str_e e_v
  | FieldDelete (e, f) -> "delete " ^ str_e e ^ "[" ^ str_e f ^ "]"
  | AssignCall (va, st, e_lst) ->
      va ^ " := " ^ str_e st ^ " ("
      ^ String.concat ~sep:", " (List.map ~f:(fun e -> str_e e) e_lst)
      ^ ")"
  | AssignECall (x, f, es) ->
      Printf.sprintf "%s := extern %s(%s)" x f (str_es es)
  | AssignNewObj va -> va ^ " := { }"
  | FieldLookup (va, eo, p) -> va ^ " := " ^ str_e eo ^ "[" ^ str_e p ^ "]"
  | AssignInObjCheck (st, e1, e2) ->
      st ^ " := " ^ str_e e1 ^ " in_obj " ^ str_e e2
  | AssignObjToList (st, e) -> st ^ " := obj_to_list " ^ str_e e
  | AssignObjFields (st, e) -> st ^ " := obj_fields " ^ str_e e
  | Exception st -> Printf.sprintf "throw \"%s\"" st
  | Assert e -> "assert (" ^ str_e e ^ ")"
  | Abort e -> "se_abort " ^ str_e e
  | SymStmt stmt -> SymStmt.str stmt

let rec js (stmt : t) : string =
  let str_es es = String.concat ~sep:", " (List.map ~f:Expr.js es) in

  match stmt.it with
  | Skip -> Printf.sprintf ""
  | Merge -> Printf.sprintf ""
  | Assign (x, e) -> Printf.sprintf "%s = %s" x (Expr.js e)
  | Print e -> Printf.sprintf "console.log( %s )" (Expr.js e)
  | If (e, s1, s2) ->
      Printf.sprintf "if ( %s ) { %s } %s" (Expr.js e) (js s1)
        (match s2 with
        | None -> Printf.sprintf ""
        | Some s -> Printf.sprintf " else { %s }" (js s))
  | Block block -> String.concat ~sep:";\n" (List.map ~f:js block)
  | While (exp, s) -> Printf.sprintf "while ( %s ) { %s } " (Expr.js exp) (js s)
  | Return exp -> Printf.sprintf "return %s" (Expr.js exp)
  | FieldAssign (e_o, e_f, e_v) ->
      Printf.sprintf "%s.%s = %s" (Expr.js e_o) (Expr.js e_f) (Expr.js e_v)
  | FieldDelete (e_o, e_f) ->
      Printf.sprintf "delete %s.%s" (Expr.js e_o) (Expr.js e_f)
  | AssignCall (x, st, es) ->
      Printf.sprintf "%s = %s( %s )" x (Expr.js st) (str_es es)
  | AssignECall (x, f, es) ->
      Printf.sprintf "%s := extern %s(%s)" x f (str_es es)
  | AssignNewObj x -> Printf.sprintf "var %s = {}" x
  | FieldLookup (x, e_o, e_f) ->
      Printf.sprintf "%s = %s.%s" x (Expr.js e_o) (Expr.js e_f)
  | AssignInObjCheck (x, e_o, e_f) ->
      Printf.sprintf "%s = %s.hasOwnProperty( %s )" x (Expr.js e_o)
        (Expr.js e_f)
  | AssignObjToList (x, e) ->
      Printf.sprintf "%s = Object.values(%s)" x (Expr.js e)
  | AssignObjFields (x, e) ->
      Printf.sprintf "%s = Object.keys(%s)" x (Expr.js e)
  | Exception st -> Printf.sprintf "throw \"%s\"" st
  | Fail e | Abort e -> Printf.sprintf "throw %s" (Expr.js e)
  | Assert _e -> failwith "Stmt: js: Assert not implemented!"
  | SymStmt _e -> failwith "Stmt: js: SymStmt not implemented!"
(*Printf.sprintf "throw %s" (Expr.js e)*)

let rec to_json (stmt : t) : string =
  (*Stmts args : rhs/ lhs / expr / obj / field/ stringvar *)
  match stmt.it with
  | Skip -> Printf.sprintf "{\"type\" : \"skip\"}"
  | Merge -> Printf.sprintf "{\"type\" : \"merge\"}"
  | Print e ->
      Printf.sprintf "{\"type\" : \"print\", \"expr\" :  %s }" (Expr.to_json e)
  | Fail e ->
      Printf.sprintf "{\"type\" : \"fail\", \"expr\" :  %s }" (Expr.to_json e)
  | Abort e ->
      Printf.sprintf "{\"type\" : \"abort\", \"expr\" :  %s }" (Expr.to_json e)
  | Assert e ->
      Printf.sprintf "{\"type\" : \"assert\", \"expr\" :  %s }" (Expr.to_json e)
  | Assign (v, exp) ->
      Printf.sprintf "{\"type\" : \"assign\", \"lhs\" :  \"%s\", \"rhs\" :  %s}"
        v (Expr.to_json exp)
  | If (e, s1, s2) ->
      Printf.sprintf
        "{\"type\" : \"condition\", \"expr\" : %s, \"then\" : %s %s}"
        (Expr.to_json e) (to_json s1)
        (match s2 with
        | Some v -> Printf.sprintf ",\"else\" : %s" (to_json v)
        | None -> "")
  | Block block ->
      Printf.sprintf "{\"type\" : \"block\", \"value\" : [ %s ]}"
        (String.concat ~sep:", " (List.map ~f:to_json block))
  | While (exp, s) ->
      Printf.sprintf "{\"type\" : \"loop\", \"expr\" : %s, \"do\" : %s }"
        (Expr.to_json exp) (to_json s)
  | Return exp ->
      Printf.sprintf "{\"type\" : \"return\", \"expr\" : %s}" (Expr.to_json exp)
  | FieldAssign (e_o, f, e_v) ->
      Printf.sprintf
        "{\"type\" : \"fieldassign\", \"obj\" : %s, \"field\" : %s, \"value\" \
         : %s}"
        (Expr.to_json e_o) (Expr.to_json f) (Expr.to_json e_v)
  | FieldDelete (e, f) ->
      Printf.sprintf
        "{\"type\" : \"fielddelete\", \"obj\" : %s, \"field\" : %s}"
        (Expr.to_json e) (Expr.to_json f)
  | AssignCall (va, st, e_lst) ->
      Printf.sprintf
        "{\"type\" : \"assigncall\", \"lhs\" : \"%s\", \"func\" : %s, \"args\" \
         : [%s]}"
        va (Expr.to_json st)
        (String.concat ~sep:", " (List.map ~f:Expr.to_json e_lst))
  | AssignECall (x, f, e_lst) ->
      Printf.sprintf
        "{\"type\" : \"assign_e_call\", \"lhs\" : \"%s\", \"func\" : \"%s\", \
         \"args\" : [%s]}"
        x f
        (String.concat ~sep:", " (List.map ~f:Expr.to_json e_lst))
  | AssignNewObj va ->
      Printf.sprintf "{\"type\" : \"assignnewobject\", \"lhs\" : \"%s\" }" va
  | FieldLookup (va, eo, p) ->
      Printf.sprintf
        "{\"type\" : \"fieldlookup\", \"lhs\" : \"%s\", \"obj\" : %s, \
         \"field\" : %s}"
        va (Expr.to_json eo) (Expr.to_json p)
  | AssignInObjCheck (st, e1, e2) ->
      Printf.sprintf
        "{\"type\" : \"assigninobjcheck\", \"lhs\" : \"%s\", \"field\" : %s, \
         \"obj\" : %s}"
        st (Expr.to_json e1) (Expr.to_json e2)
  | AssignObjToList (st, e) ->
      Printf.sprintf
        "{\"type\" : \"assigniobjtolist\", \"lhs\" : \"%s\", \"obj\" : %s}" st
        (Expr.to_json e)
  | AssignObjFields (st, e) ->
      Printf.sprintf
        "{\"type\" : \"assignobjfields\", \"lhs\" : \"%s\", \"obj\" : %s}" st
        (Expr.to_json e)
  | Exception st ->
      Printf.sprintf "{\"type\" : \"exception\", \"value\" : \"%s\"}" st
  | SymStmt st ->
      Printf.sprintf "{\"type\" : \"exception\", \"value\" : \"%s\"}"
        (SymStmt.str st)

module Pp = struct
  let to_string (stmt : t) pp : string =
    let str = pp in
    let concat es = String.concat ~sep:", " (List.map ~f:str es) in
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
    | Exception st -> Format.sprintf "throw \"%s\"" st
    | Assert e -> Format.sprintf "assert (%s)" (str e)
    | Abort e -> Format.sprintf "se_abort (%s)" (str e)
    | SymStmt stmt -> (
        match stmt with
        | SymStmt.Assume e -> sprintf "se_assume(%s)" (str e)
        | SymStmt.Evaluate (x, e) -> sprintf "%s := se_evaluate(%s)" x (str e)
        | SymStmt.Maximize (x, e) -> sprintf "%s := se_maximize(%s)" x (str e)
        | SymStmt.Minimize (x, e) -> sprintf "%s := se_minimize(%s)" x (str e)
        | SymStmt.Is_symbolic (x, e) ->
            sprintf "%s := se_is_symbolic(%s)" x (str e)
        | SymStmt.Is_sat (x, e) -> sprintf "%s := se_is_sat(%s)" x (str e)
        | SymStmt.Is_number (x, e) -> sprintf "%s := se_is_number(%s)" x (str e)
        )
end
