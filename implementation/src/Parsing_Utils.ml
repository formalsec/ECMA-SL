let parse_expr (str : string) : Expr.t =
  let lexbuf = Lexing.from_string str in
  Parser.prog_expr_target Lexer.read lexbuf

let parse_stmt (str : string) : Stmt.t =
  let lexbuf = Lexing.from_string str in
  Parser.prog_stmt_target Lexer.read lexbuf

let parse_prog (str : string) : Prog.t =
  let lexbuf = Lexing.from_string str in
  Parser.prog_target Lexer.read lexbuf

let load_file f : string =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s