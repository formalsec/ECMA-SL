open EslCore
open EslSyntax
include Parsing_utils

type 'a estart = Lexing.position -> 'a EParser.MenhirInterpreter.checkpoint
type etoken = [%import: EParser.token] [@@deriving show]

let elexer (last_token : etoken ref) (lexbuf : Lexing.lexbuf) =
  let token = ELexer.read lexbuf in
  last_token := token;
  token

let eparser (start : 'a estart) (lexbuf : Lexing.lexbuf) : EProg.t =
  let module ESLMI = EParser.MenhirInterpreter in
  let last_token = ref EParser.EOF in
  ESLMI.loop_handle
    (fun result -> result)
    (function
      | ESLMI.Rejected -> failwith "Parser rejected input"
      | ESLMI.HandlingError _e ->
        Fmt.eprintf "%a, last token: %s: %s.@." print_position lexbuf
          (show_etoken !last_token) "Error message found";
        raise EParser.Error
      | _ -> failwith "Unexpected state in failure handler!" )
    (ESLMI.lexer_lexbuf_to_supplier (elexer last_token) lexbuf)
    (start lexbuf.Lexing.lex_curr_p)

let parse_eexpr ?(file : string = "") (str : string) : EExpr.t =
  let lexbuf = init_lexbuf file str in
  EParser.entry_expr_target ELexer.read lexbuf

let parse_estmt ?(file : string = "") (str : string) : EStmt.t =
  let lexbuf = init_lexbuf file str in
  EParser.entry_stmt_target ELexer.read lexbuf

let parse_efunc ?(file : string = "") (str : string) : EFunc.t =
  let lexbuf = init_lexbuf file str in
  EParser.entry_func_target ELexer.read lexbuf

let parse_etype ?(file : string = "") (str : string) : EType.t =
  let lexbuf = init_lexbuf file str in
  EParser.entry_type_target ELexer.read lexbuf

let parse_eprog ?(file : string = "") (path : string) (str : string) : EProg.t =
  let lexbuf = Parsing_utils.init_lexbuf file str in
  let p = eparser EParser.Incremental.entry_prog_target lexbuf in
  { p with file; path }
