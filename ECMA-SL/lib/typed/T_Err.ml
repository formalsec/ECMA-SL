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
  | UnknownFunction fname -> Printf.sprintf "Cannot find function '%s'." fname
  | NExpectedArgs (nparams, nargs) ->
      Printf.sprintf "Expected %d arguments, but got %d." nparams nargs
  | DuplicatedParam param ->
      Printf.sprintf
        "Functions cannot have two parameters with the same name: '%s'." param
  | DuplicatedField fn ->
      Printf.sprintf
        "Object literals cannot have two field with the same name: '%s'." fn
  | MissingField fn ->
      Printf.sprintf "Field '%s' is missing from the object's type." fn
  | ExtraField fn ->
      Printf.sprintf "Field '%s' is not defined in the object's type." fn
  | IncompatibleField fn ->
      Printf.sprintf "Types of field '%s' are incompatible." fn
  | BadValue (tref, texpr) ->
      Printf.sprintf "Value of type '%s' is not assignable to type '%s'."
        (E_Type.str texpr) (E_Type.str tref)
  | BadExpectedType (tref, texpr) ->
      Printf.sprintf
        "Expected value of type '%s' but a value of type '%s' was provided."
        (E_Type.str tref) (E_Type.str texpr)
  | BadTypeUpdate (tref, texpr) ->
      Printf.sprintf "Variable of type '%s' cannot change its type to '%s'."
        (E_Type.str tref) (E_Type.str texpr)
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

type src_t = NoSrc | Stmt of E_Stmt.t | Func of E_Func.t

let src_region (src : src_t) : Source.region =
  match src with
  | NoSrc -> no_region
  | Stmt stmt -> stmt.at
  | Func func -> func.at

let src_str (src : src_t) : string =
  match src with
  | NoSrc -> ""
  | Stmt stmt -> E_Stmt.str stmt
  | Func func -> E_Func.str func

type tkn_t =
  | NoTkn
  | Literal of string
  | Str of string
  | Expr of E_Expr.t
  | Stmt of E_Stmt.t
  | Func of E_Func.t
  | Type of E_Type.t

let tkn_of_source (src : src_t) : tkn_t =
  match src with
  | NoSrc -> NoTkn
  | Stmt stmt -> Stmt stmt
  | Func func -> Func func

let concat_tkns (tkns : tkn_t list list) (split : string) : tkn_t list =
  let _split_tkn_f r = match r with [] -> r | r' :: _ -> Literal split :: r in
  List.fold_right (fun f r -> List.append f (_split_tkn_f r)) tkns []

let get_call_tkns (fnTkn : tkn_t) (args : E_Expr.t list) : tkn_t list =
  let argTkns = concat_tkns (List.map (fun arg -> [ Expr arg ]) args) ", " in
  List.concat [ [ fnTkn ]; [ Literal "(" ]; argTkns; [ Literal ")" ] ]

let get_type_tkns (t : E_Type.t option) : tkn_t list =
  match t with None -> [] | Some t' -> [ Literal ": "; Type t' ]

let get_expr_tkns (expr : E_Expr.t) : tkn_t list =
  match expr with
  | Val v -> [ Literal (Val.str v) ]
  | Var x -> [ Str x ]
  (* | GVar _ -> [] *)
  | Const c -> [ Literal (Operators.str_of_const c) ]
  | UnOpt (op, e) ->
      let opTkn = Literal (Operators.str_of_unopt op) in
      get_call_tkns opTkn [ e ]
  | BinOpt (op, e1, e2) ->
      let opTkn = Literal (Operators.str_of_binopt_single op) in
      get_call_tkns opTkn [ e1; e2 ]
  | EBinOpt (op, e1, e2) ->
      let opTkn = Literal (EOper.str_of_binopt_single op) in
      get_call_tkns opTkn [ e1; e2 ]
  | TriOpt (op, e1, e2, e3) ->
      let opTkn = Literal (Operators.str_of_triopt_single op) in
      get_call_tkns opTkn [ e1; e2; e3 ]
  (* | NOpt (_, _) -> [] *)
  | Call (Val (Val.Str fn), args, _) -> get_call_tkns (Str fn) args
  (* | ECall (_, _) -> [] *)
  | NewObj fes ->
      let _fe_tkn_f (fn, fe) = [ Str fn; Literal ": "; Expr fe ] in
      let feTkns = concat_tkns (List.map _fe_tkn_f fes) ", " in
      List.concat [ [ Literal "{ " ]; feTkns; [ Literal " }" ] ]
  | Lookup (oe, fe) -> [ Expr oe; Literal "["; Expr fe; Literal "]" ]
  (* | Curry (_, _) -> [] *)
  (* | Symbolic (_, _) -> [] *)
  | _ -> []

let get_stmt_tkns (stmt : E_Stmt.t) : tkn_t list =
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
      let typeTkns = get_type_tkns t in
      List.concat [ [ Str x ]; typeTkns; [ Literal " := " ]; [ Expr e ] ]
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

let get_func_tkns (func : E_Func.t) : tkn_t list =
  let _param_tkn_f (param, tparam) = Str param :: get_type_tkns tparam in
  let func' = func.it in
  let fn, fparams, freturn = (func'.name, func'.params_t, func'.return_t) in
  let funcTkns = [ Literal "function "; Str fn ] in
  let paramTkns = concat_tkns (List.map _param_tkn_f fparams) ", " in
  let retTkn = get_type_tkns freturn in
  List.concat [ funcTkns; [ Literal " (" ]; paramTkns; [ Literal ")" ]; retTkn ]

let tkn_cmp (tkn1 : tkn_t) (tkn2 : tkn_t) : bool =
  match (tkn1, tkn2) with
  | Literal tkn1', Literal tkn2' -> tkn1' = tkn2'
  | Str tkn1', Str tkn2' -> tkn1' == tkn2'
  | Type tkn1', Type tkn2' -> tkn1' == tkn2'
  | Expr tkn1', Expr tkn2' -> tkn1' == tkn2'
  | Stmt tkn1', Stmt tkn2' -> tkn1' == tkn2'
  | Func tkn1', Func tkn2' -> tkn1' == tkn2'
  | _ -> false

let tkn_is_splitable (tkn : tkn_t) : bool =
  match tkn with Expr _ | Stmt _ | Func _ -> true | _ -> false

let split_tkn (tkn : tkn_t) : tkn_t list =
  match tkn with
  | Expr tkn' -> get_expr_tkns tkn'
  | Stmt tkn' -> get_stmt_tkns tkn'
  | Func tkn' -> get_func_tkns tkn'
  | _ -> []

let rec tkn_str (tkn : tkn_t) : string =
  match tkn with
  | NoTkn -> ""
  | Literal tkn' -> tkn'
  | Str tkn' -> tkn'
  | Type tkn' -> E_Type.str tkn'
  | Expr tkn' -> String.concat "" (List.map tkn_str (get_expr_tkns tkn'))
  | Stmt tkn' -> String.concat "" (List.map tkn_str (get_stmt_tkns tkn'))
  | Func tkn' -> String.concat "" (List.map tkn_str (get_func_tkns tkn'))

type t = { errs : err_t list; src : src_t; tkn : tkn_t }

exception TypeError of t

let create ?(src : src_t = NoSrc) ?(tkn : tkn_t = NoTkn) (err : err_t) : t =
  { errs = [ err ]; src; tkn }

let raise ?(src : src_t = NoSrc) ?(tkn : tkn_t = NoTkn) (err : err_t) =
  Caml.raise (TypeError (create ~src ~tkn err))

let continue (terr : t) = Caml.raise (TypeError terr)

let update (terr : t) (err : err_t) =
  let errs =
    match terr.errs with
    | _ :: errs -> err :: errs
    | [] -> failwith "Typed ECMA-SL: T_Err.update"
  in
  Caml.raise (TypeError { terr with errs })

let push (terr : t) (err : err_t) =
  Caml.raise (TypeError { terr with errs = err :: terr.errs })

let set_tkn (terr : t) (tkn : tkn_t) =
  let terr' = { terr with tkn } in
  Caml.raise (TypeError terr')

type locData_t = {
  mutable file : string;
  mutable line : int;
  mutable left : int;
  mutable right : int;
}

type sourceData_t = {
  mutable code : string;
  mutable hgl : string;
  mutable loc : locData_t;
}

let init_source_data (src : src_t) : sourceData_t =
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

let process_empty_cause (sourceData : sourceData_t) (source : tkn_t) : unit =
  let tknStr = tkn_str source in
  let tknSize = String.length tknStr in
  sourceData.code <- tknStr;
  sourceData.hgl <- String.make tknSize '^';
  sourceData.loc.left <- 0;
  sourceData.loc.right <- tknSize - 1

let rec process_cause (sourceData : sourceData_t) (cause : tkn_t)
    (source : tkn_t) : unit =
  let _write_tkn hgl =
    let tknStr = tkn_str source in
    let tknSize = String.length tknStr in
    let _ = sourceData.code <- sourceData.code ^ tknStr in
    let _ = sourceData.hgl <- sourceData.hgl ^ String.make tknSize hgl in
    tknSize
  in
  if sourceData.loc.left >= sourceData.loc.right then
    if tkn_cmp cause source then
      let tknSize = _write_tkn '^' in
      sourceData.loc.right <- sourceData.loc.left + tknSize
    else if tkn_is_splitable source then
      List.iter (process_cause sourceData cause) (split_tkn source)
    else
      let tknSize = _write_tkn ' ' in
      sourceData.loc.left <- sourceData.loc.left + tknSize
  else ignore (_write_tkn ' ')

let find_cause (terr : t) : sourceData_t =
  let sourceData = init_source_data terr.src in
  let sourceTkn = tkn_of_source terr.src in
  (if terr.tkn = NoTkn then process_empty_cause sourceData sourceTkn
  else process_cause sourceData terr.tkn sourceTkn)
  |> fun () -> sourceData

let format_msg (errs : err_t list) : string =
  let terrHeader = Font.format "TypeError: " [ Font.red ] in
  let terrCause = Font.format "Caused by: " [ Font.yellow ] in
  match errs with
  | [] -> terrHeader ^ "???"
  | err :: errs ->
      let _side_err_str_f err = terrCause ^ err_str err ^ "\n" in
      let mainErrStr = err_str err in
      let sideErrsStr = String.concat "" (List.map _side_err_str_f errs) in
      terrHeader ^ mainErrStr ^ "\n" ^ sideErrsStr

let format_loc (loc : locData_t) : string =
  Printf.sprintf "File \"%s\", line %d, characters %d-%d:" loc.file loc.line
    loc.left (loc.right - 1)

let format_code (sourceData : sourceData_t) : string =
  Printf.sprintf "%d |   %s" sourceData.loc.line sourceData.code

let format_hgl (terrCause : sourceData_t) : string =
  let linenoSize = String.length (string_of_int terrCause.loc.line) in
  let terrCauseIdent = String.make (linenoSize + 5) ' ' in
  terrCauseIdent ^ terrCause.hgl

let format_source (terr : t) (sourceData : sourceData_t) : string =
  match terr.src with
  | NoSrc -> ""
  | _ ->
      Font.format (format_loc sourceData.loc) [ Font.faint ]
      ^ "\n" ^ format_code sourceData ^ "\n"
      ^ Font.format (format_hgl sourceData) [ Font.red ]

let format (terr : t) : string =
  let terrMsg = format_msg terr.errs in
  let sourceData = find_cause terr in
  let terrSource = format_source terr sourceData in
  Printf.sprintf "%s%s\n" terrMsg terrSource
