open Lexing

exception ImportException of string

type token = [%import: Parser.token] [@@deriving show]
type e_token = [%import: EParser.token] [@@deriving show]

let print_position (outx : Fmt.t) (lexbuf : Lexing.lexbuf) : unit =
  let pos = lexbuf.lex_curr_p in
  Printf.printf "Line number: %d. File: %s\n" pos.pos_lnum pos.pos_fname;
  Fmt.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let e_parse start (lexbuf : Lexing.lexbuf) =
  let module ESLMI = EParser.MenhirInterpreter in
  let last_token = ref EParser.EOF in
  let lexer lexbuf =
    let token = ELexer.read lexbuf in
    last_token := token;
    token
  in

  ESLMI.loop_handle
    (fun result -> result)
    (function
      | ESLMI.Rejected -> failwith "Parser rejected input"
      | ESLMI.HandlingError _e ->
        (* let csn = ESLMI.current_state_number e in *)
        Fmt.eprintf "%a, last token: %s: %s.@." print_position lexbuf
          (show_e_token !last_token) "Error message found";
        raise EParser.Error
      | _ -> failwith "Unexpected state in failure handler!" )
    (ESLMI.lexer_lexbuf_to_supplier lexer lexbuf)
    (start lexbuf.Lexing.lex_curr_p)

let parse start (lexbuf : Lexing.lexbuf) =
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = !Config.file };
  let module Core_ESLMI = Parser.MenhirInterpreter in
  let last_token = ref Parser.EOF in
  let lexer lexbuf =
    let token = Lexer.read lexbuf in
    last_token := token;
    token
  in
  Core_ESLMI.loop_handle
    (fun result -> result)
    (function
      | Core_ESLMI.Rejected -> failwith "Parser rejected input"
      | Core_ESLMI.HandlingError _e ->
        (* let csn = Core_ESLMI.current_state_number e in *)
        Fmt.eprintf "%a, last token: %s: %s.@." print_position lexbuf
          (show_token !last_token) "Error message found";
        raise Parser.Error
      | _ -> failwith "Unexpected state in failure handler!" )
    (Core_ESLMI.lexer_lexbuf_to_supplier lexer lexbuf)
    (start lexbuf.Lexing.lex_curr_p)

let init_lexbuf (fname : string) (str : string) =
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  lexbuf

module StrSet = Set.Make (String)

let parse_prog (str : string) : Prog.t =
  let lexbuf = Lexing.from_string str in
  let funcs = parse Parser.Incremental.prog_target lexbuf in
  Prog.create funcs

let parse_e_func (str : string) : EFunc.t =
  let lexbuf = Lexing.from_string str in
  EParser.e_prog_e_func_target ELexer.read lexbuf

let parse_func (str : string) : Func.t =
  let lexbuf = Lexing.from_string str in
  Parser.proc_target Lexer.read lexbuf

let parse_e_expr (str : string) : EExpr.t =
  let lexbuf = Lexing.from_string str in
  EParser.e_prog_e_expr_target ELexer.read lexbuf

let parse_e_stmt (str : string) : EStmt.t =
  let lexbuf = Lexing.from_string str in
  EParser.e_prog_e_stmt_target ELexer.read lexbuf

let parse_e_prog (fname : string) (str : string) : EProg.t =
  let lexbuf = init_lexbuf fname str in
  let prog = e_parse EParser.Incremental.e_prog_target lexbuf in
  prog

(*
  let lexbuf = Lexing.from_string str in
  EParser.e_prog_target ELexer.read lexbuf
*)

let parse_file str : Prog.t =
  let str = Io.load_file str in
  let fs = parse_prog str in
  fs

let rec resolve_imports (to_resolve : string list list) (resolved : StrSet.t)
  (path : string list) (typedefs : (string * EType.t) list)
  (funcs : EFunc.t list) (macros : EMacro.t list) :
  (string * EType.t) list * EFunc.t list * EMacro.t list =
  match (to_resolve, path) with
  | ([], _) -> (typedefs, funcs, macros)
  | ([] :: lst, file :: rest_path) ->
    let resolved = StrSet.add file resolved in
    resolve_imports lst resolved rest_path typedefs funcs macros
  | ((file :: files) :: _, _) ->
    if StrSet.mem file resolved then
      resolve_imports [ files ] resolved path typedefs funcs macros
    else if List.mem file path then
      failwith "Error resolving imports: Cyclic dependency"
    else
      let file_contents = Io.load_file file in
      let cur_prog = parse_e_prog file file_contents in
      let cur_prog_typedefs = EProg.get_typedefs_list cur_prog in
      let cur_prog_funcs = EProg.get_funcs cur_prog in
      let cur_prog_macros = EProg.get_macros cur_prog in
      resolve_imports
        (EProg.get_imports cur_prog :: to_resolve)
        resolved (file :: path)
        (cur_prog_typedefs @ typedefs)
        (cur_prog_funcs @ funcs) (cur_prog_macros @ macros)
  | ([] :: _, []) ->
    invalid_arg
      "Error resolving imports: path is empty and still some imports to \
       resolve."

let resolve_prog_imports (prog : EProg.t) : EProg.t =
  let file_name = EProg.get_file_name prog in
  let (total_typedefs, total_funcs, total_macros) =
    resolve_imports
      [ EProg.get_imports prog ]
      StrSet.empty
      [ EProg.get_file_name prog ]
      (EProg.get_typedefs_list prog)
      (EProg.get_funcs prog) (EProg.get_macros prog)
  in
  let new_prog = EProg.create [] total_typedefs total_funcs total_macros in
  EProg.set_file_name new_prog file_name;
  new_prog

let apply_prog_macros (prog : EProg.t) : EProg.t = EProg.apply_macros prog

(*
   "1.2343434e+15" -> appropriate float
*)
let parse_efloat (f_str : string) : float =
  let i = String.rindex f_str 'e' in
  let b = float_of_string (String.sub f_str 0 i) in
  let len' = String.length f_str - i - 2 in
  let exp = float_of_string (String.sub f_str (i + 2) len') in
  b *. (10. ** exp)
