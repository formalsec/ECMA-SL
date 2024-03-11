open EslCore
open EslSyntax

let load_file ?(file : string option) (path : string) : string =
  let file' = Option.value ~default:path file in
  let data = Io.read_file path in
  Source.Code.load file' data;
  data

let print_position (outx : Fmt.t) (lexbuf : Lexing.lexbuf) : unit =
  let pos = lexbuf.lex_curr_p in
  Printf.printf "Line number: %d. File: %s\n" pos.pos_lnum pos.pos_fname;
  Fmt.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let init_lexbuf (file : string) (str : string) =
  let lexbuf = Lexing.from_string str in
  { lexbuf with lex_curr_p = { lexbuf.lex_curr_p with pos_fname = file } }
