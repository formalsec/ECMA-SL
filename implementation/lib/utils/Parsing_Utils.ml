exception ImportException of string

type e_token = [%import: E_Parser.token] [@@deriving show]
type token = [%import: Parser.token] [@@deriving show]

let print_position
    (outx : Format.formatter)
    (lexbuf : Lexing.lexbuf) : unit =
  let pos = lexbuf.lex_curr_p in
  Printf.printf "Line number: %d. File: %s\n" pos.pos_lnum pos.pos_fname;  
  Format.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) 


let e_parse start (lexbuf : Lexing.lexbuf) =

  let module ESLMI = E_Parser.MenhirInterpreter in

  let last_token = ref E_Parser.EOF
  in let lexer lexbuf =
       let token = E_Lexer.read lexbuf in
       last_token := token; token in

  ESLMI.loop_handle
    (fun result -> result)   
    (function ESLMI.Rejected -> failwith "Parser rejected input"
            | ESLMI.HandlingError e ->
              (* let csn = ESLMI.current_state_number e in *)
              Format.eprintf "%a, last token: %s: %s.@."
                print_position lexbuf
                (show_e_token !last_token)
                "Error message found";
              raise E_Parser.Error
            | _ -> failwith "Unexpected state in failure handler!")
    (ESLMI.lexer_lexbuf_to_supplier lexer lexbuf)
    (start lexbuf.Lexing.lex_curr_p)



let parse start (lexbuf : Lexing.lexbuf) =

  let module Core_ESLMI = Parser.MenhirInterpreter in

  let last_token = ref Parser.EOF
  in let lexer lexbuf =
       let token = Lexer.read lexbuf in
       last_token := token; token in

  Core_ESLMI.loop_handle
    (fun result -> result)   
    (function Core_ESLMI.Rejected -> failwith "Parser rejected input"
            | Core_ESLMI.HandlingError e ->
              (* let csn = Core_ESLMI.current_state_number e in *)
              Format.eprintf "%a, last token: %s: %s.@."
                print_position lexbuf
                (show_token !last_token)
                "Error message found";
              raise Parser.Error
            | _ -> failwith "Unexpected state in failure handler!")
    (Core_ESLMI.lexer_lexbuf_to_supplier lexer lexbuf)
    (start lexbuf.Lexing.lex_curr_p)

let init_lexbuf (fname: string) (str : string) =
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  lexbuf 


module StrSet = Set.Make(String)

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

let parse_e_prog (fname: string) (str : string) : E_Prog.t =
  let lexbuf = init_lexbuf fname str in 
  let prog = e_parse E_Parser.Incremental.e_prog_target lexbuf in
  prog

(*
  let lexbuf = Lexing.from_string str in
  E_Parser.e_prog_target E_Lexer.read lexbuf
*) 

let load_file f : string =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let write_file (s : string) (f : string) : unit =
  let oc = open_out f in
  output_string oc s;
  close_out oc

let parse_file str : Prog.t =
  let str = load_file str in
  let fs = parse_prog str in
  fs


let rec resolve_imports (to_resolve : string list list) (resolved : StrSet.t) (path : string list) (funcs : E_Func.t list) : E_Func.t list =
  match (to_resolve, path) with
  | ([], _) -> funcs
  | ([] :: lst, file :: rest_path) ->
    let resolved =  StrSet.add file resolved in
    resolve_imports lst resolved rest_path funcs
  | ((file :: files)::_, _) ->
    if (StrSet.mem file resolved)
    then resolve_imports [files] resolved path funcs
    else (if (List.mem file path)
          then failwith "Error resolving imports: Cyclic dependency"
          else (
            let file_contents = load_file file in
            let cur_prog = parse_e_prog file file_contents in
            let cur_prog_funcs = E_Prog.get_funcs cur_prog in
            resolve_imports (E_Prog.get_imports cur_prog :: to_resolve) resolved (file :: path) (funcs @ cur_prog_funcs)
          ))
  | ([]::_, []) -> invalid_arg "Error resolving imports: path is empty and still some imports to resolve."

let resolve_prog_imports (prog : E_Prog.t) : E_Prog.t =
  let file_name = E_Prog.get_file_name prog in
  let total_funcs = resolve_imports  [ E_Prog.get_imports prog ] StrSet.empty [ E_Prog.get_file_name prog ] (E_Prog.get_funcs prog) in
  let new_prog = E_Prog.create [] total_funcs in
  E_Prog.set_file_name new_prog file_name; new_prog
