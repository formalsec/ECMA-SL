open E_Expr
open E_Stmt
open E_Func
open Source

type err =
  | UnknownVar of string
  | UnknownFunction of string
  | DuplicatedParam of string
  | DuplicatedField of string
  | MissingArgs of int * int
  | MissingField of string
  | ExtraField of string
  | ExpectedObjectExpr of E_Expr.t
  | BadExpectedType of E_Type.t * E_Type.t
  | BadAssignment of E_Type.t * E_Type.t
  | BadReturn of E_Type.t * E_Type.t
  | BadArgument of E_Type.t * E_Type.t
  | BadOp of string * E_Type.t list
  | BadField of string * E_Type.t * E_Type.t
  | BadLookup of E_Type.t * string

let err_str (err : err) : string =
  match err with
  | UnknownVar x -> Printf.sprintf "Cannot find variable '%s'." x
  | UnknownFunction fn -> Printf.sprintf "Cannot find function '%s'." fn
  | DuplicatedParam pn ->
      Printf.sprintf
        "Functions cannot have two parameters with the same name: '%s'." pn
  | DuplicatedField fn ->
      Printf.sprintf
        "Object literals cannot have two field with the same name: '%s'." fn
  | MissingArgs (nparams, nargs) ->
      Printf.sprintf "Expected %d arguments, but got %d." nparams nargs
  | MissingField fn ->
      Printf.sprintf "Field '%s' is missing from the object's type." fn
  | ExtraField fn ->
      Printf.sprintf "Field '%s' does not exist in the object's type" fn
  | ExpectedObjectExpr e ->
      Printf.sprintf "Expression '%s' is expected to be of type object."
        (E_Expr.str e)
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
  | BadField (fn, ft, texpr) ->
      Printf.sprintf
        "Value of type '%s' cannot be assigned to field '%s' of type '%s'."
        (E_Type.str texpr) fn (E_Type.str ft)
  | BadLookup (tobj, fn) ->
      Printf.sprintf "Field '%s' does not exist on type '%s'." fn
        (E_Type.str tobj)

type src = NoSource | Stmt of E_Stmt.t | Func of E_Func.t

type token =
  | NoToken
  | Literal of string
  | Str of string
  | Expr of E_Expr.t
  | Stmt of E_Stmt.t
  | Func of E_Func.t
  | Type of E_Type.t

type t = { err : err; src : src; cs : token }

exception TypeError of t

let create ?(src : src = NoSource) ?(cs : token = NoToken) (err : err) : t =
  { err; src; cs }

let raise ?(src : src = NoSource) ?(cs : token = NoToken) (err : err) : 'a =
  Caml.raise (TypeError (create err ~src ~cs))

let get_err (terr : t) : err = terr.err
let get_src (terr : t) : src = terr.src
let get_cs (terr : t) : token = terr.cs

let src_region (src : src) : Source.region =
  match src with
  | NoSource -> Source.no_region
  | Stmt stmt -> stmt.at
  | Func func -> func.at

let src_to_token (src : src) : token =
  match src with
  | NoSource -> NoToken
  | Stmt stmt -> Stmt stmt
  | Func func -> Func func

let src_str (src : src) : string =
  match src with
  | NoSource -> ""
  | Stmt stmt -> E_Stmt.str stmt
  | Func func -> E_Func.str func

let concat_tokens (tkns : token list list) (s : string) : token list =
  let separator_token r = match r with [] -> r | r' :: _ -> Literal s :: r in
  List.fold_right (fun p r -> List.append p (separator_token r)) tkns []

let call_tokens (args : E_Expr.t list) (op_str : string) : token list =
  let arg_tkns = List.map (fun arg -> [ Expr arg ]) args in
  let arg_tkns = concat_tokens arg_tkns ", " in
  let tkns = Literal op_str :: Literal "(" :: arg_tkns in
  List.append tkns [ Literal ")" ]

let get_expr_tokens (expr : E_Expr.t) : token list =
  match expr with
  | Val v -> [ Literal (Val.str v) ]
  | Var x -> [ Str x ]
  (* | GVar _ -> [] *)
  | Const c -> [ Literal (Operators.str_of_const c) ]
  | UnOpt (op, e) ->
      let op_str = Operators.str_of_unopt op in
      call_tokens [ e ] op_str
  | BinOpt (op, e1, e2) ->
      let op_str = Operators.str_of_binopt_single op in
      call_tokens [ e1; e2 ] op_str
  | EBinOpt (op, e1, e2) ->
      let op_str = EOper.str_of_binopt_single op in
      call_tokens [ e1; e2 ] op_str
  | TriOpt (op, e1, e2, e3) ->
      let op_str = Operators.str_of_triopt_single op in
      call_tokens [ e1; e2; e3 ] op_str
  (* | NOpt (_, _) -> [] *)
  | Call (Val (Val.Str fn), args, _) -> call_tokens args fn
  (* | ECall (_, _) -> [] *)
  | NewObj fes ->
      let ftoken_fun (fn, fe) = [ Str fn; Literal ": "; Expr fe ] in
      let ftkns = List.map ftoken_fun fes in
      let tkns = Literal "{ " :: concat_tokens ftkns ", " in
      List.append tkns [ Literal " }" ]
  | Lookup (oe, fe) -> [ Expr oe; Literal "["; Expr fe; Literal "]" ]
  (* | Curry (_, _) -> [] *)
  (* | Symbolic (_, _) -> [] *)
  | default -> []

let get_stmt_tokens (stmt : E_Stmt.t) : token list =
  match stmt.it with
  | Skip -> []
  (* | Fail _ -> [] *)
  (* | Throw _ -> [] *)
  | Print e -> [ Literal "print "; Expr e ]
  (* | Assume _ -> [] *)
  (* | Assert _ -> [] *)
  | Return e -> [ Literal "return "; Expr e ]
  (* | Wrapper (_, _) -> [] *)
  | Assign (x, t, e) ->
      let ttkns =
        match t with None -> [] | Some t' -> [ Literal ": "; Type t' ]
      in
      let tkns = Str x :: ttkns in
      List.append tkns [ Literal " := "; Expr e ]
  (* | GlobAssign (_, _) -> [] *)
  | Block stmts -> []
  | If (e, _, _, _, _) ->
      let threedots = Font.format "..." [ Font.faint ] in
      [ Literal "if ("; Expr e; Literal ") { "; Literal threedots ]
  (* | EIf (_, _) -> [] *)
  | While (e, _) ->
      let threedots = Font.format "..." [ Font.faint ] in
      [ Literal "while ("; Expr e; Literal ") { "; Literal threedots ]
  (* | ForEach (_, _, _, _, _) -> [] *)
  | FieldAssign (oe, fe, e) ->
      [ Expr oe; Literal "["; Expr fe; Literal "]"; Literal " := "; Expr e ]
  (* | FieldDelete (_, _) -> [] *)
  | ExprStmt expr -> [ Expr expr ]
  (* | RepeatUntil (_, _, _) -> [] *)
  (* | MatchWith (_, _) -> [] *)
  (* | MacroApply (_, _) -> [] *)
  (* | Switch (_, _, _, _) -> [] *)
  (* | Lambda (_, _, _, _, _) -> [] *)
  | default -> []

let get_func_tokens (func : E_Func.t) : token list =
  let fn, fparams, fr = (func.it.name, func.it.params_t, func.it.return_t) in
  let param_token_fun (tparam : string * E_Type.t option) : token list =
    Str (fst tparam)
    :: (match snd tparam with None -> [] | Some t -> [ Str ": "; Type t ])
  in
  let param_tkns = concat_tokens (List.map param_token_fun fparams) ", " in
  let ret_tkn = match fr with None -> [] | Some t -> [ Str " : "; Type t ] in
  let ftkns = Str "function " :: Str fn :: Str " (" :: param_tkns in
  let ftkns = List.append ftkns [ Str ")" ] in
  let ftkns = List.append ftkns ret_tkn in
  ftkns

let token_cmp (tkn1 : token) (tkn2 : token) : bool =
  match (tkn1, tkn2) with
  | Literal tkn1', Literal tkn2' -> tkn1' = tkn2'
  | Str tkn1', Str tkn2' -> tkn1' == tkn2'
  | Type tkn1', Type tkn2' -> tkn1' == tkn2'
  | Expr tkn1', Expr tkn2' -> tkn1' == tkn2'
  | Stmt tkn1', Stmt tkn2' -> tkn1' == tkn2'
  | Func tkn1', Func tkn2' -> tkn1' == tkn2'
  | default -> false

let token_is_splitable (tkn : token) : bool =
  match tkn with
  | NoToken -> false
  | Literal _ -> false
  | Str _ -> false
  | Type _ -> false
  | Expr _ -> true
  | Stmt _ -> true
  | Func _ -> true

let split_token (tkn : token) : token list =
  match tkn with
  | Expr tkn' -> get_expr_tokens tkn'
  | Stmt tkn' -> get_stmt_tokens tkn'
  | Func tkn' -> get_func_tokens tkn'
  | default -> []

let rec token_str (tkn : token) : string =
  match tkn with
  | NoToken -> ""
  | Literal tkn' -> tkn'
  | Str tkn' -> tkn'
  | Type tkn' -> E_Type.str tkn'
  | Expr tkn' -> String.concat "" (List.map token_str (get_expr_tokens tkn'))
  | Stmt tkn' -> String.concat "" (List.map token_str (get_stmt_tokens tkn'))
  | Func tkn' -> String.concat "" (List.map token_str (get_func_tokens tkn'))

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

let init_source_data (src : src) : source_data =
  let region = src_region src in
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

let fill_empty_source_data (cause_data : source_data) : source_data =
  let text_size = String.length cause_data.code in
  if cause_data.loc.left > cause_data.loc.right then (
    cause_data.loc.left <- cause_data.loc.right;
    cause_data.loc.right <- cause_data.loc.left + text_size;
    cause_data.hgl <- String.make text_size '^');
  cause_data

let rec process_cause (source_data : source_data) (cause : token)
    (source : token) : unit =
  let write_token_fun (hgl : char) : int =
    let tkn_str = token_str source in
    let tkn_size = String.length tkn_str in
    let _ = source_data.code <- source_data.code ^ tkn_str in
    let _ = source_data.hgl <- source_data.hgl ^ String.make tkn_size hgl in
    tkn_size
  in
  if source_data.loc.left >= source_data.loc.right then
    if token_cmp cause source then
      let tkn_size = write_token_fun '^' in
      source_data.loc.right <- source_data.loc.left + tkn_size
    else if token_is_splitable source then
      let process_cause_fun = process_cause source_data cause in
      List.iter (fun tkn -> process_cause_fun tkn) (split_token source)
    else
      let tkn_size = write_token_fun ' ' in
      source_data.loc.left <- source_data.loc.left + tkn_size
  else ignore (write_token_fun ' ')

let find_cause (terr : t) : source_data =
  let source_data = init_source_data terr.src in
  let first_tkn = src_to_token terr.src in
  let _ = process_cause source_data terr.cs first_tkn in
  source_data
(* fill_empty_source_data source_data *)

let format_loc (loc : loc_data) : string =
  Printf.sprintf "File \"%s\", line %d, characters %d-%d:" loc.file loc.line
    loc.left (loc.right - 1)

let format_code (source_data : source_data) : string =
  Printf.sprintf "%d |   %s" source_data.loc.line source_data.code

let format_hgl (terr_cause : source_data) : string =
  let lineno_size = String.length (string_of_int terr_cause.loc.line) in
  let terr_cause_ident = String.make (lineno_size + 5) ' ' in
  terr_cause_ident ^ terr_cause.hgl

let format_source (terr : t) (source_data : source_data) : string =
  match terr.src with
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
