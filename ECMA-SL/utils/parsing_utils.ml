open Lexing

type 'a start = position -> 'a Parser.MenhirInterpreter.checkpoint
type 'a estart = position -> 'a EParser.MenhirInterpreter.checkpoint
type token = [%import: Parser.token] [@@deriving show]
type etoken = [%import: EParser.token] [@@deriving show]

let load_file (file : string) : string =
  let data = Io.read_file file in
  Source.Code.load file data;
  data

let load_dependency (file : Id.t) : string =
  try load_file file.it
  with _ -> Eslerr.(compile ~src:(ErrSrc.at file) (UnknownDependency file.it))

let print_position (outx : Fmt.t) (lexbuf : Lexing.lexbuf) : unit =
  let pos = lexbuf.lex_curr_p in
  Printf.printf "Line number: %d. File: %s\n" pos.pos_lnum pos.pos_fname;
  Fmt.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let lexer (last_token : token ref) (lexbuf : Lexing.lexbuf) =
  let token = Lexer.read lexbuf in
  last_token := token;
  token

let elexer (last_token : etoken ref) (lexbuf : Lexing.lexbuf) =
  let token = ELexer.read lexbuf in
  last_token := token;
  token

let parser (start : 'a start) (lexbuf : Lexing.lexbuf) =
  let module Core_ESLMI = Parser.MenhirInterpreter in
  let last_token = ref Parser.EOF in
  Core_ESLMI.loop_handle
    (fun result -> result)
    (function
      | Core_ESLMI.Rejected -> failwith "Parser rejected input"
      | Core_ESLMI.HandlingError _e ->
        Fmt.eprintf "%a, last token: %s: %s.@." print_position lexbuf
          (show_token !last_token) "Error message found";
        raise Parser.Error
      | _ -> failwith "Unexpected state in failure handler!" )
    (Core_ESLMI.lexer_lexbuf_to_supplier (lexer last_token) lexbuf)
    (start lexbuf.Lexing.lex_curr_p)

let eparser (start : 'a estart) (lexbuf : Lexing.lexbuf) =
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

let init_lexbuf (file : string) (str : string) =
  let lexbuf = Lexing.from_string str in
  { lexbuf with lex_curr_p = { lexbuf.lex_curr_p with pos_fname = file } }

let parse_expr ?(file : string = "") (str : string) : Expr.t =
  let lexbuf = init_lexbuf file str in
  Parser.entry_expr_target Lexer.read lexbuf

let parse_stmt ?(file : string = "") (str : string) : Stmt.t =
  let lexbuf = init_lexbuf file str in
  Parser.entry_stmt_target Lexer.read lexbuf

let parse_func ?(file : string = "") (str : string) : Func.t =
  let lexbuf = init_lexbuf file str in
  Parser.entry_func_target Lexer.read lexbuf

let parse_prog ?(file : string = "") (str : string) : Prog.t =
  let lexbuf = init_lexbuf file str in
  parser Parser.Incremental.entry_prog_target lexbuf

let parse_eexpr ?(file : string = "") (str : string) : EExpr.t =
  let lexbuf = init_lexbuf file str in
  EParser.entry_expr_target ELexer.read lexbuf

let parse_estmt ?(file : string = "") (str : string) : EStmt.t =
  let lexbuf = init_lexbuf file str in
  EParser.entry_stmt_target ELexer.read lexbuf

let parse_efunc ?(file : string = "") (str : string) : EFunc.t =
  let lexbuf = init_lexbuf file str in
  EParser.entry_func_target ELexer.read lexbuf

let parse_eprog ?(file : string = "") (str : string) : EProg.t =
  let lexbuf = init_lexbuf file str in
  eparser EParser.Incremental.entry_prog_target lexbuf

module SSet = Set.Make (String)

let rec resolve_imports (resolved : SSet.t) (paths : string list)
  (unresolved : Id.t list list) (tdefs : EType.tdef list) (funcs : EFunc.t list)
  (macros : EMacro.t list) : EType.tdef list * EFunc.t list * EMacro.t list =
  match (unresolved, paths) with
  | ([], _) -> (tdefs, funcs, macros)
  | ([] :: unresolved', file :: paths') ->
    let resolved = SSet.add file resolved in
    resolve_imports resolved paths' unresolved' tdefs funcs macros
  | ((file :: files) :: _, _) ->
    if SSet.mem file.it resolved then
      resolve_imports resolved paths [ files ] tdefs funcs macros
    else if not (List.mem file.it paths) then
      let eprog = load_dependency file |> parse_eprog ~file:file.it in
      resolve_imports resolved (file.it :: paths)
        (EProg.imports eprog :: unresolved)
        (EProg.tdefs_lst eprog @ tdefs)
        (EProg.funcs_lst eprog @ funcs)
        (EProg.macros_lst eprog @ macros)
    else Eslerr.(compile ~src:(ErrSrc.at file) (CyclicDependency file.it))
  | ([] :: _, []) -> Eslerr.(internal __FUNCTION__ (Expecting "non-empty path"))

let resolve_eprog_imports (prog : EProg.t) : EProg.t =
  let file = EProg.file prog in
  let (typedefs, funcs, macros) =
    resolve_imports SSet.empty
      [ EProg.file prog ]
      [ EProg.imports prog ]
      (EProg.tdefs_lst prog) (EProg.funcs_lst prog) (EProg.macros_lst prog)
  in
  EProg.create file [] typedefs funcs macros

let apply_eprog_macros (prog : EProg.t) : EProg.t = EProg.apply_macros prog
