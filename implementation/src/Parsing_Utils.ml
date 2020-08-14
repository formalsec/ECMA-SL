
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

module SS = Set.Make(String)

let rec resolve_imports_2 (to_resolve : string list) (resolved : SS.t) (path : string list) : E_Func.t list = 
  match to_resolve with 
    | [] -> [] 
    | file :: files ->
      if (SS.mem file resolved) 
        then resolve_imports_2 files resolved path 
        else (if (List.mem file path)
                then failwith "Resolving imports: Cyclic dependency"
                else (
                  let file_contents = load_file file in
                  let cur_prog = parse_e_prog file_contents in
                  let cur_prog_funcs = E_Prog.get_funcs cur_prog in 
                  let cur_prog_import_funcs = resolve_imports_2 cur_prog.imports resolved (file :: path) in 
                  let resolved =  SS.add file resolved in 
                  let rest_files_funcs = resolve_imports_2 files resolved path in 
                  cur_prog_funcs @ cur_prog_import_funcs @ rest_files_funcs
                ))


let rec resolve_imports_3 (to_resolve : string list list) (resolved : SS.t) (path : string list) (funcs : E_Func.t list) : E_Func.t list = 
  match (to_resolve, path) with 
    | ([], _) -> funcs
    | ([] :: lst, file :: rest_path) ->
      let resolved =  SS.add file resolved in 
      resolve_imports_3 lst resolved rest_path funcs 
    | ((file :: files):: _, _) ->
      if (SS.mem file resolved) 
        then resolve_imports_3 [files] resolved path funcs 
        else (if (List.mem file path)
              then failwith "Resolving imports: Cyclic dependency"
                else (
                  let file_contents = load_file file in
                  let cur_prog = parse_e_prog file_contents in
                  let cur_prog_funcs = E_Prog.get_funcs cur_prog in 
                  resolve_imports_3 (cur_prog.imports :: to_resolve) resolved (file :: path) (funcs @ cur_prog_funcs) 
                ))
    | ([]::_, []) -> invalid_arg "Error resolving imports: path is empty and still some imports to resolve."

let my_resolve_imports (prog : E_Prog.t) : E_Func.t list = 
   resolve_imports_3  [ prog.imports ] SS.empty [ prog.file_name ] (E_Prog.get_funcs prog)