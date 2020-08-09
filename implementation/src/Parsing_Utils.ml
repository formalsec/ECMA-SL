
let parse_prog (str : string) : Prog.t  =
  let lexbuf = Lexing.from_string str in
  let funcs= Parser.prog_target Lexer.read lexbuf in
  Prog.create funcs

let parse_e_expr (str : string) : E_Expr.t =
  let lexbuf = Lexing.from_string str in
  E_Parser.e_prog_e_expr_target E_Lexer.read lexbuf

let parse_e_stmt (str : string) : E_Stmt.t =
  let lexbuf = Lexing.from_string str in
  E_Parser.e_prog_e_stmt_target E_Lexer.read lexbuf

let parse_e_prog (str : string) : E_Prog.t =
  let lexbuf = Lexing.from_string str in
  E_Parser.e_prog_target E_Lexer.read lexbuf

let load_file f : string =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let parse_file str : Prog.t =
  let str = load_file str in
  let fs = parse_prog str in
  fs

let rec resolve_imports (prog : E_Prog.t) : E_Prog.t =
  let imported_funcs = List.fold_left (fun acc i -> (resolve_import i) @ acc) [] prog.imports in
  E_Prog.add_funcs prog imported_funcs; prog

and resolve_import (import : string) : E_Func.t list =
  let e_prog_contents = load_file import in
  let e_prog = parse_e_prog e_prog_contents in
  let e_prog_resolved = resolve_imports e_prog in
  let funcs = Hashtbl.fold (fun fname func acc -> acc @ [func]) e_prog_resolved.funcs [] in
  funcs
