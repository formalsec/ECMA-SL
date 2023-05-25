open E_Expr
open E_Stmt
open E_Func
open Source

type err_t =
  | UnknownVar of string
  | UnknownFunction of string
  | NExpectedArgs of int * int
  | DuplicatedParam of string
  | DuplicatedField of string
  | MissingField of string
  | ExtraField of string
  | IncompatibleField of string
  | BadValue of E_Type.t * E_Type.t
  | BadExpectedType of E_Type.t * E_Type.t
  | BadTypeUpdate of E_Type.t * E_Type.t
  | BadReturn of E_Type.t * E_Type.t
  | BadArgument of E_Type.t * E_Type.t
  | BadOperand of E_Type.t * E_Type.t
  | BadType of string option * E_Type.t
  | BadPossibleType of string option * E_Type.t
  | BadLookup of string * E_Type.t
  | NoOverlapComp of E_Type.t * E_Type.t

let err_str (err : err_t) : string =
  match err with
  | UnknownVar x -> Printf.sprintf "Cannot find variable '%s'." x
  | UnknownFunction fn -> Printf.sprintf "Cannot find function '%s'." fn
  | NExpectedArgs (nparams, nargs) ->
      Printf.sprintf "Expected %d arguments, but got %d." nparams nargs
  | DuplicatedParam pn ->
      Printf.sprintf
        "Functions cannot have two parameters with the same name: '%s'." pn
  | DuplicatedField fn ->
      Printf.sprintf
        "Object literals cannot have two field with the same name: '%s'." fn
  | MissingField fn ->
      Printf.sprintf "Field '%s' is missing from the object's type." fn
  | ExtraField fn ->
      Printf.sprintf "Field '%s' is not defined in the object's type." fn
  | IncompatibleField fn ->
      Printf.sprintf "Types of field '%s' are incompatible." fn
  | BadValue (tvar, texpr) ->
      Printf.sprintf "Value of type '%s' is not assignable to type '%s'."
        (E_Type.str texpr) (E_Type.str tvar)
  | BadExpectedType (tref, texpr) ->
      Printf.sprintf
        "Expected value of type '%s' but a value of type '%s' was provided."
        (E_Type.str tref) (E_Type.str texpr)
  | BadTypeUpdate (tvar, texpr) ->
      Printf.sprintf "Variable of type '%s' cannot change its type to '%s'."
        (E_Type.str tvar) (E_Type.str texpr)
  | BadReturn (tret, texpr) ->
      Printf.sprintf "Value of type '%s' cannot be returned by a '%s' function."
        (E_Type.str texpr) (E_Type.str tret)
  | BadArgument (tparam, targ) ->
      Printf.sprintf
        "Argument of type '%s' is not assignable to a parameter of type '%s'."
        (E_Type.str targ) (E_Type.str tparam)
  | BadOperand (tparam, targ) ->
      Printf.sprintf
        "Argument of type '%s' is not assignable to a operand of type '%s'."
        (E_Type.str targ) (E_Type.str tparam)
  | BadType (x, t) ->
      let name = Option.default "Object" x in
      Printf.sprintf "'%s' is of type '%s'." name (E_Type.str t)
  | BadPossibleType (x, t) ->
      let name = Option.default "Object" x in
      Printf.sprintf "'%s' is possible of type '%s'." name (E_Type.str t)
  | BadLookup (fn, tobj) ->
      Printf.sprintf "Field '%s' does not exist on type '%s'." fn
        (E_Type.str tobj)
  | NoOverlapComp (t1, t2) ->
      Printf.sprintf
        "This comparison appears to be unintentional because the types '%s' \
         and '%s' have no overlap."
        (E_Type.str t1) (E_Type.str t2)

type src_t = NoSource | Stmt of E_Stmt.t | Func of E_Func.t

let src_region (src : src_t) : Source.region =
  match src with
  | NoSource -> no_region
  | Stmt stmt -> stmt.at
  | Func func -> func.at

let src_str (src : src_t) : string =
  match src with
  | NoSource -> ""
  | Stmt stmt -> E_Stmt.str stmt
  | Func func -> E_Func.str func

type token_t =
  | NoToken
  | Literal of string
  | Str of string
  | Expr of E_Expr.t
  | Stmt of E_Stmt.t
  | Func of E_Func.t
  | Type of E_Type.t

let token_of_source (src : src_t) : token_t =
  match src with
  | NoSource -> NoToken
  | Stmt stmt -> Stmt stmt
  | Func func -> Func func

let concat_tokens (tkns : token_t list list) (s : string) : token_t list =
  let separator_token r = match r with [] -> r | r' :: _ -> Literal s :: r in
  List.fold_right (fun p r -> List.append p (separator_token r)) tkns []

let get_call_tokens (fn_tkn : token_t) (args : E_Expr.t list) : token_t list =
  let arg_tkns = List.map (fun arg -> [ Expr arg ]) args in
  let arg_tkns = concat_tokens arg_tkns ", " in
  let tkns = fn_tkn :: Literal "(" :: arg_tkns in
  List.append tkns [ Literal ")" ]

let get_expr_tokens (expr : E_Expr.t) : token_t list =
  match expr with
  | Val v -> [ Literal (Val.str v) ]
  | Var x -> [ Str x ]
  (* | GVar _ -> [] *)
  | Const c -> [ Literal (Operators.str_of_const c) ]
  | UnOpt (op, e) ->
      let op_tkn = Literal (Operators.str_of_unopt op) in
      get_call_tokens op_tkn [ e ]
  | BinOpt (op, e1, e2) ->
      let op_tkn = Literal (Operators.str_of_binopt_single op) in
      get_call_tokens op_tkn [ e1; e2 ]
  | EBinOpt (op, e1, e2) ->
      let op_tkn = Literal (EOper.str_of_binopt_single op) in
      get_call_tokens op_tkn [ e1; e2 ]
  | TriOpt (op, e1, e2, e3) ->
      let op_tkn = Literal (Operators.str_of_triopt_single op) in
      get_call_tokens op_tkn [ e1; e2; e3 ]
  (* | NOpt (_, _) -> [] *)
  | Call (Val (Val.Str fn), args, _) -> get_call_tokens (Str fn) args
  (* | ECall (_, _) -> [] *)
  | NewObj fes ->
      let ftoken_fun (fn, fe) = [ Str fn; Literal ": "; Expr fe ] in
      let ftkns = List.map ftoken_fun fes in
      let tkns = Literal "{ " :: concat_tokens ftkns ", " in
      List.append tkns [ Literal " }" ]
  | Lookup (oe, fe) -> [ Expr oe; Literal "["; Expr fe; Literal "]" ]
  (* | Curry (_, _) -> [] *)
  (* | Symbolic (_, _) -> [] *)
  | _ -> []

let get_stmt_tokens (stmt : E_Stmt.t) : token_t list =
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
      let type_tokens_fun (t : E_Type.t) = [ Literal ": "; Type t ] in
      let tkns = match t with None -> [] | Some t' -> type_tokens_fun t' in
      List.append (Str x :: tkns) [ Literal " := "; Expr e ]
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
  | _ -> []

let get_func_tokens (func : E_Func.t) : token_t list =
  let fn, fparams, fr = (func.it.name, func.it.params_t, func.it.return_t) in
  let param_token_fun tparam =
    Str (fst tparam)
    :: (match snd tparam with None -> [] | Some t -> [ Str ": "; Type t ])
  in
  let param_tkns = concat_tokens (List.map param_token_fun fparams) ", " in
  let ret_tkn = match fr with None -> [] | Some t -> [ Str " : "; Type t ] in
  let ftkns = Str "function " :: Str fn :: Str " (" :: param_tkns in
  let ftkns = List.append ftkns [ Str ")" ] in
  let ftkns = List.append ftkns ret_tkn in
  ftkns

let token_cmp (tkn1 : token_t) (tkn2 : token_t) : bool =
  match (tkn1, tkn2) with
  | Literal tkn1', Literal tkn2' -> tkn1' = tkn2'
  | Str tkn1', Str tkn2' -> tkn1' == tkn2'
  | Type tkn1', Type tkn2' -> tkn1' == tkn2'
  | Expr tkn1', Expr tkn2' -> tkn1' == tkn2'
  | Stmt tkn1', Stmt tkn2' -> tkn1' == tkn2'
  | Func tkn1', Func tkn2' -> tkn1' == tkn2'
  | _ -> false

let token_is_splitable (tkn : token_t) : bool =
  match tkn with
  | NoToken -> false
  | Literal _ -> false
  | Str _ -> false
  | Type _ -> false
  | Expr _ -> true
  | Stmt _ -> true
  | Func _ -> true

let split_token (tkn : token_t) : token_t list =
  match tkn with
  | Expr tkn' -> get_expr_tokens tkn'
  | Stmt tkn' -> get_stmt_tokens tkn'
  | Func tkn' -> get_func_tokens tkn'
  | _ -> []

let rec token_str (tkn : token_t) : string =
  match tkn with
  | NoToken -> ""
  | Literal tkn' -> tkn'
  | Str tkn' -> tkn'
  | Type tkn' -> E_Type.str tkn'
  | Expr tkn' -> String.concat "" (List.map token_str (get_expr_tokens tkn'))
  | Stmt tkn' -> String.concat "" (List.map token_str (get_stmt_tokens tkn'))
  | Func tkn' -> String.concat "" (List.map token_str (get_func_tokens tkn'))

type t = { errs : err_t list; src : src_t; tkn : token_t }

exception TypeError of t

let create ?(src : src_t = NoSource) ?(tkn : token_t = NoToken)
    (errs : err_t list) : t =
  { errs; src; tkn }

let raise ?(src : src_t = NoSource) ?(tkn : token_t = NoToken) (err : err_t) =
  Caml.raise (TypeError (create ~src ~tkn [ err ]))

let continue (terr : t) = Caml.raise (TypeError terr)

let update (terr : t) (err : err_t) =
  let errs =
    match terr.errs with
    | [] -> failwith "Typed ECMA-SL: T_Err.update"
    | _ :: errs -> err :: errs
  in
  Caml.raise (TypeError { terr with errs })

let push (terr : t) (err : err_t) =
  Caml.raise (TypeError { terr with errs = err :: terr.errs })

let set_token (terr : t) (tkn : token_t) =
  let terr' = { terr with tkn } in
  Caml.raise (TypeError terr')

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

let init_source_data (src : src_t) : source_data =
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

let process_empty_cause (source_data : source_data) (source : token_t) : unit =
  let tkn_str = token_str source in
  let tkn_size = String.length tkn_str in
  source_data.code <- tkn_str;
  source_data.hgl <- String.make tkn_size '^';
  source_data.loc.left <- 0;
  source_data.loc.right <- tkn_size - 1

let rec process_cause (source_data : source_data) (cause : token_t)
    (source : token_t) : unit =
  let write_token_fun hgl =
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
  let source_tkn = token_of_source terr.src in
  let _ =
    if terr.tkn = NoToken then process_empty_cause source_data source_tkn
    else process_cause source_data terr.tkn source_tkn
  in
  source_data

let format_msg (errs : err_t list) : string =
  let terr_header = Font.format "TypeError: " [ Font.red ] in
  let terr_cause = Font.format "Caused by: " [ Font.yellow ] in
  match errs with
  | [] -> terr_header ^ "???"
  | err :: errs ->
      let side_err_str_fun err = terr_cause ^ err_str err ^ "\n" in
      let str_main_err = err_str err in
      let str_side_err = String.concat "" (List.map side_err_str_fun errs) in
      terr_header ^ str_main_err ^ "\n" ^ str_side_err

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
  | _ ->
      Font.format (format_loc source_data.loc) [ Font.faint ]
      ^ "\n" ^ format_code source_data ^ "\n"
      ^ Font.format (format_hgl source_data) [ Font.red ]

let format (terr : t) : string =
  let terr_msg = format_msg terr.errs in
  let source_data = find_cause terr in
  let terr_source = format_source terr source_data in
  Printf.sprintf "%s%s\n" terr_msg terr_source
