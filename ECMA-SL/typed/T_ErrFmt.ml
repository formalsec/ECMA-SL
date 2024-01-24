open EExpr
open EStmt
open EFunc
open EPat
open Source

type kind_t =
  | Error
  | Warning

let kind_err (kind : kind_t) : string =
  match kind with Error -> "TypeError" | Warning -> "Type Warning"

let kind_font (kind : kind_t) : Font.t =
  match kind with Error -> Font.Red | Warning -> Font.Yellow

type tkn_t =
  | NoTkn
  | Lit of string
  | Str of string
  | Type of EType.t
  | Expr of EExpr.t
  | Stmt of EStmt.t
  | Func of EFunc.t
  | Pat of EPat.t
  | PatVal of EPat.pv

let tkn_region (src : tkn_t) : Source.region =
  match src with
  | Stmt stmt -> stmt.at
  | Func func -> func.at
  | Pat pat -> pat.at
  | _ -> no_region

let concat_tkns (tkns : tkn_t list list) (split : string) : tkn_t list =
  let _split_tkn_f r = match r with [] -> r | _r' :: _ -> Lit split :: r in
  List.fold_right (fun f r -> List.append f (_split_tkn_f r)) tkns []

let type_tkns (t : EType.t option) : tkn_t list =
  match t with None -> [] | Some t' -> [ Lit ": "; Type t' ]

let call_tkns (fnTkn : tkn_t) (args : EExpr.t list) : tkn_t list =
  let argTkns = concat_tkns (List.map (fun arg -> [ Expr arg ]) args) ", " in
  List.concat [ [ fnTkn ]; [ Lit "(" ]; argTkns; [ Lit ")" ] ]

let unop_tkns (op : Operator.unopt) (e : EExpr.t) : tkn_t list =
  let opTkn = Lit (Operator.str_of_unopt_single op) in
  match op with
  | Operator.Neg -> [ opTkn; Expr e ]
  | Operator.LogicalNot -> [ opTkn; Expr e ]
  | Operator.BitwiseNot -> [ opTkn; Expr e ]
  | _ -> call_tkns opTkn [ e ]

let binop_tkns (op : Operator.binopt) (e1 : EExpr.t) (e2 : EExpr.t) : tkn_t list
    =
  let opStr = Operator.str_of_binopt_single op in
  let opTkn = Lit (" " ^ opStr ^ " ") in
  match op with
  | Operator.Plus -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.Minus -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.Times -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.Div -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.Modulo -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.Eq -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.Gt -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.Lt -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.Ge -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.Le -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.LogicalAnd -> [ Lit "("; Expr e1; opTkn; Expr e2; Lit ")" ]
  | Operator.LogicalOr -> [ Lit "("; Expr e1; opTkn; Expr e2; Lit ")" ]
  | Operator.BitwiseAnd -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.BitwiseOr -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.BitwiseXor -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.ShiftLeft -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.ShiftRight -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.ShiftRightLogical -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.ObjectMem -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.ListMem -> [ Expr e1; opTkn; Expr e2 ]
  | Operator.Pow -> [ Expr e1; opTkn; Expr e2 ]
  | _ -> call_tkns (Lit opStr) [ e1; e2 ]

let triop_tkns (op : Operator.triopt) (e1 : EExpr.t) (e2 : EExpr.t)
  (e3 : EExpr.t) : tkn_t list =
  let opTkn = Lit (Operator.str_of_triopt_single op) in
  match op with _ -> call_tkns opTkn [ e1; e2; e3 ]

let nopt_tkns (op : Operator.nopt) (es : EExpr.t list) : tkn_t list =
  let exprTkns = concat_tkns (List.map (fun arg -> [ Expr arg ]) es) ", " in
  match op with
  | Operator.TupleExpr -> List.concat [ [ Lit "(" ]; exprTkns; [ Lit ")" ] ]
  | _ -> []

let expr_tkns (expr : EExpr.t) : tkn_t list =
  match expr.it with
  | Val v -> [ Lit (Val.str v) ]
  | Var x -> [ Str x ]
  (* | GVar _ -> [] *)
  | Const c -> [ Lit (Operator.str_of_const c) ]
  | UnOpt (op, e) -> unop_tkns op e
  | BinOpt (op, e1, e2) -> binop_tkns op e1 e2
  | TriOpt (op, e1, e2, e3) -> triop_tkns op e1 e2 e3
  | NOpt (op, es) -> nopt_tkns op es
  | Call ({ it = Val (Val.Str fn); _ }, args, _) -> call_tkns (Str fn) args
  (* | ECall (_, _) -> [] *)
  | NewObj fes ->
    let _fe_tkn_f (fn, fe) = [ Str fn.it; Lit ": "; Expr fe ] in
    let feTkns = concat_tkns (List.map _fe_tkn_f fes) ", " in
    List.concat [ [ Lit "{ " ]; feTkns; [ Lit " }" ] ]
  | Lookup (oe, fe) -> [ Expr oe; Lit "["; Expr fe; Lit "]" ]
  (* | Curry (_, _) -> [] *)
  (* | Symbolic (_, _) -> [] *)
  | _ -> []

let stmt_tkns (stmt : EStmt.t) : tkn_t list =
  let threedots = Font.str_text_err [ Font.Faint ] "..." in
  match stmt.it with
  | Skip -> []
  (* | Fail _ -> [] *)
  (* | Throw _ -> [] *)
  | Print e -> [ Lit "print "; Expr e ]
  (* | Assume _ -> [] *)
  (* | Assert _ -> [] *)
  | Return None -> [ Lit "return" ]
  | Return (Some e) -> [ Lit "return "; Expr e ]
  (* | Wrapper (_, _) -> [] *)
  | Assign (x, t, e) ->
    let typeTkns = type_tkns t in
    List.concat [ [ Str x.it ]; typeTkns; [ Lit " := " ]; [ Expr e ] ]
  (* | GlobAssign (_, _) -> [] *)
  | Block _stmts -> []
  (* | If (e, _, _, _, _) -> [ Lit "if ("; Expr e; Lit ") { "; Lit threedots ] *)
  (* | EIf (_, _) -> [] *)
  | While (e, _) -> [ Lit "while ("; Expr e; Lit ") { "; Lit threedots ]
  (* | ForEach (_, _, _, _, _) -> [] *)
  | FieldAssign (oe, fe, e) ->
    [ Expr oe; Lit "["; Expr fe; Lit "]"; Lit " := "; Expr e ]
  (* | FieldDelete (_, _) -> [] *)
  | ExprStmt expr -> [ Expr expr ]
  (* | RepeatUntil (_, _, _) -> [] *)
  | MatchWith (e, _) -> [ Lit "match "; Expr e; Lit " with "; Lit threedots ]
  (* | MacroApply (_, _) -> [] *)
  (* | Switch (_, _, _, _) -> [] *)
  (* | Lambda (_, _, _, _, _) -> [] *)
  | _ -> []

let func_tkns (func : EFunc.t) : tkn_t list =
  let _param_tkn_f (param, tparam) = Str param.it :: type_tkns tparam in
  let func' = func.it in
  let (fn, fparams, freturn) = (func'.name, func'.tparams, func'.treturn) in
  let funcTkns = [ Lit "function "; Str fn.it ] in
  let paramTkns = concat_tkns (List.map _param_tkn_f fparams) ", " in
  let retTkn = type_tkns freturn in
  List.concat [ funcTkns; [ Lit "(" ]; paramTkns; [ Lit ")" ]; retTkn ]

let pat_tkns (pat : EPat.t) : tkn_t list =
  let threedots = Font.str_text_err [ Font.Faint ] "..." in
  let _pat_fld_tkn_f (s, patVal) = [ Str s.it; Lit ": "; PatVal patVal ] in
  let _pat_tkns pat =
    match pat with
    | ObjPat (patFlds, _) ->
      let patFldTkns = concat_tkns (List.map _pat_fld_tkn_f patFlds) ", " in
      List.concat [ [ Lit "{ " ]; patFldTkns; [ Lit " }" ] ]
    | DefaultPat -> [ Lit "default" ]
  in
  let patTkns = _pat_tkns pat.it in
  List.concat [ [ Lit "| " ]; patTkns; [ Lit (" -> { " ^ threedots) ] ]

let tkn_cmp (tkn1 : tkn_t) (tkn2 : tkn_t) : bool =
  match (tkn1, tkn2) with
  | (Lit tkn1', Lit tkn2') -> tkn1' = tkn2'
  | (Str tkn1', Str tkn2') -> tkn1' == tkn2'
  | (Type tkn1', Type tkn2') -> tkn1' == tkn2'
  | (Expr tkn1', Expr tkn2') -> tkn1' == tkn2'
  | (Stmt tkn1', Stmt tkn2') -> tkn1' == tkn2'
  | (Func tkn1', Func tkn2') -> tkn1' == tkn2'
  | (Pat tkn1', Pat tkn2') -> tkn1' == tkn2'
  | (PatVal tkn1', PatVal tkn2') -> tkn1' == tkn2'
  | _ -> false

let tkn_is_splitable (tkn : tkn_t) : bool =
  match tkn with Expr _ | Stmt _ | Func _ | Pat _ -> true | _ -> false

let split_tkn (tkn : tkn_t) : tkn_t list =
  match tkn with
  | Expr tkn' -> expr_tkns tkn'
  | Stmt tkn' -> stmt_tkns tkn'
  | Func tkn' -> func_tkns tkn'
  | Pat tkn' -> pat_tkns tkn'
  | _ -> []

let rec tkn_str (tkn : tkn_t) : string =
  match tkn with
  | NoTkn -> ""
  | Lit l -> l
  | Str s -> s
  | Type t -> EType.str t
  | Expr expr -> String.concat "" (List.map tkn_str (expr_tkns expr))
  | Stmt stmt -> String.concat "" (List.map tkn_str (stmt_tkns stmt))
  | Func func -> String.concat "" (List.map tkn_str (func_tkns func))
  | Pat pat -> String.concat "" (List.map tkn_str (pat_tkns pat))
  | PatVal pv -> EPat.pv_str pv

let tkn_str_size (tkn : tkn_t) : string * int =
  let tknStr = tkn_str tkn in
  let cleanTknStr = Font.clean tknStr in
  (tknStr, String.length cleanTknStr)

type locData_t =
  { region : region
  ; mutable file : string
  ; mutable line : int
  ; mutable left : int
  ; mutable right : int
  }

type srcData_t =
  { mutable code : string
  ; mutable hgl : string
  ; mutable locData : locData_t
  }

let init_source_data (src : tkn_t) : srcData_t =
  let region = tkn_region src in
  { code = ""
  ; hgl = ""
  ; locData =
      { region
      ; file = region.file
      ; line = region.left.line
      ; left = region.left.column
      ; right = region.left.column
      }
  }

let process_empty_cause (srcData : srcData_t) (src : tkn_t) : unit =
  let (tknStr, tknSize) = tkn_str_size src in
  srcData.code <- tknStr;
  srcData.hgl <- String.make tknSize '^';
  srcData.locData.left <- 0;
  srcData.locData.right <- tknSize - 1

let rec process_cause (srcData : srcData_t) (tkn : tkn_t) (src : tkn_t) : unit =
  let _write_tkn hgl =
    let (tknStr, tknSize) = tkn_str_size src in
    let _ = srcData.code <- srcData.code ^ tknStr in
    let _ = srcData.hgl <- srcData.hgl ^ String.make tknSize hgl in
    tknSize
  in
  if srcData.locData.left >= srcData.locData.right then
    if tkn_cmp tkn src then
      let tknSize = _write_tkn '^' in
      srcData.locData.right <- srcData.locData.left + tknSize
    else if tkn_is_splitable src then
      List.iter (process_cause srcData tkn) (split_tkn src)
    else
      let tknSize = _write_tkn ' ' in
      srcData.locData.left <- srcData.locData.left + tknSize
  else ignore (_write_tkn ' ')

let process_source (tkn : tkn_t) (src : tkn_t) : srcData_t =
  let _process_cause srcData =
    if tkn = NoTkn then process_empty_cause srcData src
    else process_cause srcData tkn src
  in
  let srcData = init_source_data src in
  _process_cause srcData |> fun () -> srcData

let format_loc (locData : locData_t) : string =
  if locData.region = no_region then ""
  else
    Printf.sprintf "File \"%s\", line %d, characters %d-%d:\n" locData.file
      locData.line locData.left (locData.right - 1)

let format_code (sourceData : srcData_t) : string =
  let header =
    if sourceData.locData.region = no_region then "      "
    else Printf.sprintf "%d |   " sourceData.locData.line
  in
  Printf.sprintf "%s%s\n" header sourceData.code

let format_hgl (terrCause : srcData_t) : string =
  let linenoSize = String.length (string_of_int terrCause.locData.line) in
  let terrCauseIdent = String.make (linenoSize + 5) ' ' in
  terrCauseIdent ^ terrCause.hgl

let format_source (kind : kind_t) (tkn : tkn_t) (src : tkn_t) : string =
  let srcData = process_source tkn src in
  match src with
  | NoTkn -> ""
  | _ ->
    Font.str_text_err [ Font.Italic; Font.Faint ] (format_loc srcData.locData)
    ^ format_code srcData
    ^ Font.str_text_err [ kind_font kind ] (format_hgl srcData)

let format_msg (kind : kind_t) (msgs : string list) : string =
  let font = kind_font kind in
  let terrHeader = Font.str_text_err [ font ] (kind_err kind ^ ": ") in
  let terrCause = Font.str_text_err [ font; Font.Faint ] "Caused by: " in
  match msgs with
  | [] -> terrHeader ^ "???"
  | mainErrStr :: sideErrs ->
    let _side_err_str_f errStr = terrCause ^ errStr ^ "\n" in
    let sideErrsStr = String.concat "" (List.map _side_err_str_f sideErrs) in
    terrHeader ^ mainErrStr ^ "\n" ^ sideErrsStr
