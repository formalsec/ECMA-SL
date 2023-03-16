open E_Expr
open E_Stmt
open E_Func
open Source

type err =
  | UnknownVar of string
  | UnknownFunction of string
  | BadExpectedType of E_Type.t * E_Type.t
  | DuplicatedParam of string
  | BadAssignment of E_Type.t * E_Type.t
  | BadReturn of E_Type.t * E_Type.t
  | MissingArgs of int * int
  | BadArgument of E_Type.t * E_Type.t
  | BadOp of string * E_Type.t list

let err_str (err : err) : string =
  match err with
  | UnknownVar var -> Printf.sprintf "Cannot find variable '%s'." var
  | UnknownFunction fname -> Printf.sprintf "Cannot find function '%s'." fname
  | BadExpectedType (tref, texpr) ->
      Printf.sprintf
        "Expected value of type '%s' but a value of type '%s' was provided."
        (E_Type.str tref) (E_Type.str texpr)
  | DuplicatedParam param -> Printf.sprintf "Duplicated parameter '%s'." param
  | BadAssignment (tvar, texpr) ->
      Printf.sprintf "Value of type '%s' is not assignable to type '%s'."
        (E_Type.str texpr) (E_Type.str tvar)
  | BadReturn (tret, texpr) ->
      Printf.sprintf "Value of type '%s' cannot be returned by a '%s' function."
        (E_Type.str texpr) (E_Type.str tret)
  | MissingArgs (nparams, nargs) ->
      Printf.sprintf "Expected %d arguments, but got %d." nparams nargs
  | BadArgument (tparam, targ) ->
      Printf.sprintf
        "Argument of type '%s' is not assignable to a parameter of type '%s'."
        (E_Type.str targ) (E_Type.str tparam)
  | BadOp (op_str, texprs) ->
      Printf.sprintf
        "Arguments of type '(%s)' are not compatible with the '%s' operator."
        (String.concat " * " (List.map (fun t -> E_Type.str t) texprs))
        op_str

type source = Stmt of E_Stmt.t | Func of E_Func.t

type token =
  | Literal of string
  | Str of string
  | Type of E_Type.t
  | Expr of E_Expr.t
  | Stmt of E_Stmt.t
  | Func of E_Func.t

type t = { source : source; cause : token; err : err }

exception TypeError of t

let source_reg (source : source) : Source.region =
  match source with Stmt stmt -> stmt.at | Func func -> func.at

let source_to_token (source : source) : token =
  match source with Stmt stmt -> Stmt stmt | Func func -> Func func

let source_str (source : source) : string =
  match source with
  | Stmt stmt -> E_Stmt.str stmt
  | Func func -> E_Func.str func

let get_expr_tokens (expr : E_Expr.t) : token list =
  match expr with
  | Val v -> [ Literal (Val.str v) ]
  | Var var -> [ Str var ]
  (* | GVar _ -> [] *)
  | Const const -> [ Literal (Operators.str_of_const const) ]
  (* | UnOpt (_, _) -> [] *)
  (* | BinOpt (_, _, _) -> [] *)
  (* | EBinOpt (_, _, _) -> [] *)
  (* | TriOpt (_, _, _, _) -> [] *)
  (* | NOpt (_, _) -> [] *)
  | Call (Val (Val.Str fname), args, _) ->
      let arg_tokens =
        let tokens = List.map (fun arg -> Expr arg) args in
        let comma_token r = match r with [] -> r | r' :: _ -> Str ", " :: r in
        List.fold_right (fun p r -> p :: comma_token r) tokens []
      in
      let call_tokens = [ Str fname; Literal " (" ] in
      let call_tokens = List.append call_tokens arg_tokens in
      List.append call_tokens [ Literal ")" ]
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
  | Literal token' -> token'
  | Str token' -> token'
  | Type token' -> E_Type.str token'
  | Expr token' ->
      String.concat "" (List.map token_str (get_expr_tokens token'))
  | Stmt token' ->
      String.concat "" (List.map token_str (get_stmt_tokens token'))
  | Func token' ->
      String.concat "" (List.map token_str (get_func_tokens token'))

type source_data = {
  mutable file : string;
  mutable line : int;
  mutable left : int;
  mutable right : int;
}

type cause_data = {
  mutable text : string;
  mutable hgl : string;
  mutable source : source_data;
}

let init_cause_data (terr : source) : cause_data =
  let region = match terr with Func func -> func.at | Stmt stmt -> stmt.at in
  {
    text = "";
    hgl = "";
    source =
      {
        file = region.left.file;
        line = region.left.line;
        left = region.left.column;
        right = region.left.column;
      };
  }

let fill_cause_data_if_empty (cause_data : cause_data) : cause_data =
  let text_size = String.length cause_data.text in
  if cause_data.source.left >= cause_data.source.right then (
    cause_data.source.left <- cause_data.source.right;
    cause_data.source.right <- cause_data.source.left + text_size;
    cause_data.hgl <- String.make text_size '^');
  cause_data

let rec process_cause (data : cause_data) (cause : token) (source : token) :
    unit =
  let token_str = token_str source in
  let token_size = String.length token_str in
  if data.source.left >= data.source.right then
    if token_cmp cause source then (
      data.text <- data.text ^ token_str;
      data.hgl <- data.hgl ^ String.make token_size '^';
      data.source.right <- data.source.left + token_size)
    else if token_is_splitable source then
      List.iter
        (fun token -> process_cause data cause token)
        (split_token source)
    else (
      data.text <- data.text ^ token_str;
      data.hgl <- data.hgl ^ String.make token_size ' ';
      data.source.left <- data.source.left + token_size)
  else (
    data.text <- data.text ^ token_str;
    data.hgl <- data.hgl ^ String.make token_size ' ')

let find_cause (terr : t) : cause_data =
  let cause_data = init_cause_data terr.source in
  let first_token = source_to_token terr.source in
  let _ = process_cause cause_data terr.cause first_token in
  fill_cause_data_if_empty cause_data

let format_source (source : source_data) : string =
  Printf.sprintf "File \"%s\", line %d, characters %d-%d:" source.file
    source.line source.left source.right

let format_text (terr_cause : cause_data) : string =
  Printf.sprintf "%d |   %s" terr_cause.source.line terr_cause.text

let format_hgl (terr_cause : cause_data) : string =
  let lineno_size = String.length (string_of_int terr_cause.source.line) in
  let terr_cause_ident = String.make (lineno_size + 5) ' ' in
  terr_cause_ident ^ terr_cause.hgl

let format (terr : t) : string =
  let terr_header = Font.format "TypeError" [ Font.red ] in
  let terr_msg = err_str terr.err in
  let terr_cause = find_cause terr in
  let terr_source =
    Font.format (format_source terr_cause.source) [ Font.faint ]
  in
  let terr_text = format_text terr_cause in
  let terr_hgl = Font.format (format_hgl terr_cause) [ Font.red ] in
  Printf.sprintf "%s: %s\n%s\n%s\n%s\n" terr_header terr_msg terr_source
    terr_text terr_hgl