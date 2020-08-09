
let parse_prog (str : string) : Prog.t  =
  let lexbuf = Lexing.from_string str in
  let funcs= Parser.prog_target Lexer.read lexbuf in
  Prog.create [] funcs

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
