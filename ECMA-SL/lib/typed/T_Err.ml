open E_Expr
open E_Stmt
open E_Func
open Source

type err =
  | UnknownVar of string
  | UnknownFunction of string
  | DuplicatedParam of string
  | MissingArgs of int * int
  | BadExpectedType of E_Type.t * E_Type.t
  | BadAssignment of E_Type.t * E_Type.t
  | BadReturn of E_Type.t * E_Type.t
  | BadArgument of E_Type.t * E_Type.t
  | BadOp of string * E_Type.t list

let err_str (err : err) : string =
  match err with
  | UnknownVar var -> Printf.sprintf "Cannot find variable '%s'." var
  | UnknownFunction fname -> Printf.sprintf "Cannot find function '%s'." fname
  | DuplicatedParam param -> Printf.sprintf "Duplicated parameter '%s'." param
  | MissingArgs (nparams, nargs) ->
      Printf.sprintf "Expected %d arguments, but got %d." nparams nargs
  | BadExpectedType (tref, texpr) ->
      Printf.sprintf
        "Expected value of type '%s' but a value of type '%s' was provided."
        (E_Type.str tref) (E_Type.str texpr)
  | BadAssignment (tvar, texpr) ->
      Printf.sprintf "Value of type '%s' is not assignable to type '%s'."
        (E_Type.str texpr) (E_Type.str tvar)
  | BadReturn (tret, texpr) ->
      Printf.sprintf "Value of type '%s' cannot be returned by a '%s' function."
        (E_Type.str texpr) (E_Type.str tret)
  | BadArgument (tparam, targ) ->
      Printf.sprintf
        "Argument of type '%s' is not assignable to a parameter of type '%s'."
        (E_Type.str targ) (E_Type.str tparam)
  | BadOp (op_str, texprs) ->
      Printf.sprintf
        "Arguments of type '(%s)' are not compatible with the '%s' operator."
        (String.concat ", " (List.map (fun t -> E_Type.str t) texprs))
        op_str

type source = NoSource | Stmt of E_Stmt.t | Func of E_Func.t

type token =
  | NoToken
  | Literal of string
  | Str of string
  | Type of E_Type.t
  | Expr of E_Expr.t
  | Stmt of E_Stmt.t
  | Func of E_Func.t

type t = { err : err; source : source; cause : token }

exception TypeError of t

let create ?(src : source = NoSource) ?(cs : token = NoToken) (err : err) : t =
  { err; source = src; cause = cs }

let raise ?(src : source = NoSource) ?(cs : token = NoToken) (err : err) : 'a =
  raise (TypeError (create err ~src ~cs))

let source_region (source : source) : Source.region =
  match source with
  | NoSource -> Source.no_region
  | Stmt stmt -> stmt.at
  | Func func -> func.at

let source_to_token (source : source) : token =
  match source with
  | NoSource -> NoToken
  | Stmt stmt -> Stmt stmt
  | Func func -> Func func

let source_str (source : source) : string =
  match source with
  | NoSource -> ""
  | Stmt stmt -> E_Stmt.str stmt
  | Func func -> E_Func.str func

let concat_tokens (tokens : token list) (s : string) : token list =
  let separator_token r = match r with [] -> r | r' :: _ -> Literal s :: r in
  List.fold_right (fun p r -> p :: separator_token r) tokens []

let call_tokens (args : E_Expr.t list) (op_str : string) : token list =
  let arg_tokens_list = List.map (fun arg -> Expr arg) args in
  let arg_tokens = concat_tokens arg_tokens_list ", " in
  let tokens = [ Literal op_str; Literal "(" ] in
  let tokens = List.append tokens arg_tokens in
  List.append tokens [ Literal ")" ]

let get_expr_tokens (expr : E_Expr.t) : token list =
  match expr with
  | Val v -> [ Literal (Val.str v) ]
  | Var var -> [ Str var ]
  (* | GVar _ -> [] *)
  | Const const -> [ Literal (Operators.str_of_const const) ]
  | UnOpt (op, expr) ->
      let op_str = Operators.str_of_unopt op in
      call_tokens [ expr ] op_str
  | BinOpt (op, expr1, expr2) ->
      let op_str = Operators.str_of_binopt_single op in
      call_tokens [ expr1; expr2 ] op_str
  | EBinOpt (op, expr1, expr2) ->
      let op_str = EOper.str_of_binopt_single op in
      call_tokens [ expr1; expr2 ] op_str
  | TriOpt (op, expr1, expr2, expr3) ->
      let op_str = Operators.str_of_triopt_single op in
      call_tokens [ expr1; expr2; expr3 ] op_str
  (* | NOpt (_, _) -> [] *)
  | Call (Val (Val.Str fname), args, _) -> call_tokens args fname
  (* | ECall (_, _) -> [] *)
  (* | NewObj _ -> [] *)
  (* | Lookup (_, _) -> [] *)
  (* | Curry (_, _) -> [] *)
  (* | Symbolic (_, _) -> [] *)
  | default -> []

let get_stmt_tokens (stmt : E_Stmt.t) : token list =
  match stmt.it with
  | Skip -> []
  (* | Fail _ -> [] *)
  (* | Throw _ -> [] *)
  | Print expr -> [ Literal "print "; Expr expr ]
  (* | Assume _ -> [] *)
  (* | Assert _ -> [] *)
  | Return expr -> [ Literal "return "; Expr expr ]
  (* | Wrapper (_, _) -> [] *)
  | Assign (var, tvar, expr) ->
      let assign_tokens =
        List.append [ Str var ]
          (match tvar with
          | None -> []
          | Some tvar' -> [ Literal ": "; Type tvar' ])
      in
      List.append assign_tokens [ Literal " := "; Expr expr ]
  (* | GlobAssign (_, _) -> [] *)
  | Block stmts -> []
  | If (expr, _, _, _, _) ->
      let threedots = Font.format "..." [ Font.faint ] in
      [ Literal "if ("; Expr expr; Literal ") { "; Literal threedots ]
  (* | EIf (_, _) -> [] *)
  (* | While (_, _) -> [] *)
  (* | ForEach (_, _, _, _, _) -> [] *)
  (* | FieldAssign (_, _, _) -> [] *)
  (* | FieldDelete (_, _) -> [] *)
  | ExprStmt expr -> [ Expr expr ]
  (* | RepeatUntil (_, _, _) -> [] *)
  (* | MatchWith (_, _) -> [] *)
  (* | MacroApply (_, _) -> [] *)
  (* | Switch (_, _, _, _) -> [] *)
  (* | Lambda (_, _, _, _, _) -> [] *)
  | default -> []

let get_func_tokens (func : E_Func.t) : token list =
  let param_token (tparam : string * E_Type.t option) : token list =
    match snd tparam with
    | None -> [ Str (fst tparam) ]
    | Some t -> [ Str (fst tparam); Str ": "; Type t ]
  in
  let param_tokens =
    let tokens = List.map param_token func.it.params_t in
    let comma_token r = match r with [] -> r | r' :: _ -> Str ", " :: r in
    List.fold_right (fun p r -> List.append p (comma_token r)) tokens []
  in
  let return_token =
    match func.it.return_t with None -> [] | Some t -> [ Str " : "; Type t ]
  in
  let func_tokens = [ Str "function "; Str func.it.name; Str " (" ] in
  let func_tokens = List.append func_tokens param_tokens in
  let func_tokens = List.append func_tokens [ Str ")" ] in
  let func_tokens = List.append func_tokens return_token in
  func_tokens

let token_cmp (token1 : token) (token2 : token) : bool =
  match (token1, token2) with
  | Literal token1', Literal token2' -> token1' = token2'
  | Str token1', Str token2' -> token1' == token2'
  | Type token1', Type token2' -> token1' == token2'
  | Expr token1', Expr token2' -> token1' == token2'
  | Stmt token1', Stmt token2' -> token1' == token2'
  | Func token1', Func token2' -> token1' == token2'
  | default -> false

let token_is_splitable (token : token) : bool =
  match token with
  | Expr _ -> true
  | Stmt _ -> true
  | Func _ -> true
  | default -> false

let split_token (token : token) : token list =
  match token with
  | Expr token' -> get_expr_tokens token'
  | Stmt token' -> get_stmt_tokens token'
  | Func token' -> get_func_tokens token'
  | default -> []

let rec token_str (token : token) : string =
  match token with
  | NoToken -> ""
  | Literal token' -> token'
  | Str token' -> token'
  | Type token' -> E_Type.str token'
  | Expr token' ->
      String.concat "" (List.map token_str (get_expr_tokens token'))
  | Stmt token' ->
      String.concat "" (List.map token_str (get_stmt_tokens token'))
  | Func token' ->
      String.concat "" (List.map token_str (get_func_tokens token'))

type loc_data = {
  mutable file : string;
  mutable line : int;
  mutable left : int;
  mutable right : int;
}

type source_data = {
  mutable code : string;
  mutable hgl : string;
  mutable loc : loc_data;
}

let init_source_data (source : source) : source_data =
  let region = source_region source in
  {
    code = "";
    hgl = "";
    loc =
      {
        file = region.left.file;
        line = region.left.line;
        left = region.left.column;
        right = region.left.column;
      };
  }

let fill_source_data_if_empty (cause_data : source_data) : source_data =
  let text_size = String.length cause_data.code in
  if cause_data.loc.left >= cause_data.loc.right then (
    cause_data.loc.left <- cause_data.loc.right;
    cause_data.loc.right <- cause_data.loc.left + text_size;
    cause_data.hgl <- String.make text_size '^');
  cause_data

let rec process_cause (source_data : source_data) (cause : token)
    (source : token) : unit =
  let token_str = token_str source in
  let token_size = String.length token_str in
  if source_data.loc.left >= source_data.loc.right then
    if token_cmp cause source then (
      source_data.code <- source_data.code ^ token_str;
      source_data.hgl <- source_data.hgl ^ String.make token_size '^';
      source_data.loc.right <- source_data.loc.left + token_size)
    else if token_is_splitable source then
      List.iter
        (fun token -> process_cause source_data cause token)
        (split_token source)
    else (
      source_data.code <- source_data.code ^ token_str;
      source_data.hgl <- source_data.hgl ^ String.make token_size ' ';
      source_data.loc.left <- source_data.loc.left + token_size)
  else (
    source_data.code <- source_data.code ^ token_str;
    source_data.hgl <- source_data.hgl ^ String.make token_size ' ')

let find_cause (terr : t) : source_data =
  let source_data = init_source_data terr.source in
  let first_token = source_to_token terr.source in
  let _ = process_cause source_data terr.cause first_token in
  fill_source_data_if_empty source_data

let format_loc (loc : loc_data) : string =
  Printf.sprintf "File \"%s\", line %d, characters %d-%d:" loc.file loc.line
    loc.left loc.right

let format_code (source_data : source_data) : string =
  Printf.sprintf "%d |   %s" source_data.loc.line source_data.code

let format_hgl (terr_cause : source_data) : string =
  let lineno_size = String.length (string_of_int terr_cause.loc.line) in
  let terr_cause_ident = String.make (lineno_size + 5) ' ' in
  terr_cause_ident ^ terr_cause.hgl

let format_source (terr : t) (source_data : source_data) : string =
  match terr.source with
  | NoSource -> ""
  | default ->
      Font.format (format_loc source_data.loc) [ Font.faint ]
      ^ "\n" ^ format_code source_data ^ "\n"
      ^ Font.format (format_hgl source_data) [ Font.red ]

let format (terr : t) : string =
  let terr_header = Font.format "TypeError" [ Font.red ] in
  let terr_msg = err_str terr.err in
  let source_data = find_cause terr in
  let terr_source = format_source terr source_data in
  Printf.sprintf "%s: %s\n%s\n" terr_header terr_msg terr_source
