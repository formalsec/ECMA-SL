let not_implemented () : 'a = failwith "not implemented in eslerr"
let threedots = Font.str_format_err [ Font.Faint ] "..."

type token =
  | NoTkn
  | Index of int
  | Lit of string
  | Str of string
  | Val of Val.t
  | Expr of Expr.t
  | Stmt of Stmt.t
  | Func of Func.t

let separate_tkns (sep : string) (tkns : token list) : token list =
  let sep_f tkn = function [] -> [ tkn ] | acc -> tkn :: Lit sep :: acc in
  List.fold_right sep_f tkns []

let exprs_to_tokens (es : Expr.t list) : token list =
  List.map (fun e -> Expr e) es

let tokens_call (call_tkn : token) (args_tkn : token list) : token list =
  let argTkns = separate_tkns ", " args_tkn in
  List.concat [ [ call_tkn ]; [ Lit "(" ]; argTkns; [ Lit ")" ] ]

let tokens_unopt (op : Operator.unopt) (e_tkn : token) : token list =
  let open Operator in
  let op_tkn = Lit (str_of_unopt_single op) in
  let args_tkn () = [ e_tkn ] in
  if is_infix_unopt op then [ op_tkn; e_tkn ]
  else tokens_call op_tkn (args_tkn ())

let tokens_binopt (op : Operator.binopt) (e1_tkn : token) (e2_tkn : token) :
  token list =
  let open Operator in
  let op_tkn = Lit (str_of_binopt_single op) in
  let args_tkn () = [ e1_tkn; e2_tkn ] in
  if is_infix_binopt op then [ e1_tkn; op_tkn; e2_tkn ]
  else tokens_call op_tkn (args_tkn ())

let tokens_triopt (op : Operator.triopt) (e1_tkn : token) (e2_tkn : token)
  (e3_tkn : token) : token list =
  let open Operator in
  let op_tkn = Lit (str_of_triopt_single op) in
  let args_tkn () = [ e1_tkn; e2_tkn; e3_tkn ] in
  tokens_call op_tkn (args_tkn ())

let tokens_nopt (op : Operator.nopt) (es_tkn : token list) : token list =
  let open Operator in
  let sep_tkns sep = separate_tkns sep es_tkn in
  match op with
  | NAryLogicalAnd -> sep_tkns " && "
  | NAryLogicalOr -> sep_tkns " || "
  | ArrayExpr -> List.concat [ [ Lit "[|" ]; sep_tkns ", "; [ Lit "|]" ] ]
  | ListExpr -> List.concat [ [ Lit "[" ]; sep_tkns ", "; [ Lit "]" ] ]
  | TupleExpr -> List.concat [ [ Lit "(" ]; sep_tkns ", "; [ Lit ")" ] ]

let tokens_expr (expr : Expr.t) : token list =
  let open Expr in
  match expr with
  | Val v -> [ Val v ]
  | Var x -> [ Str x ]
  | UnOpt (op, e) -> tokens_unopt op (Expr e)
  | BinOpt (op, e1, e2) -> tokens_binopt op (Expr e1) (Expr e2)
  | TriOpt (op, e1, e2, e3) -> tokens_triopt op (Expr e1) (Expr e2) (Expr e3)
  | NOpt (op, es) -> tokens_nopt op (exprs_to_tokens es)
  | Curry _ -> not_implemented ()
  | Symbolic _ -> not_implemented ()

let tokens_stmt (stmt : Stmt.t) : token list =
  let open Stmt in
  match stmt.it with
  | Skip -> []
  | Merge -> []
  | Debug -> []
  | Block _ -> []
  | Print e -> [ Lit "print "; Expr e ]
  | Return e -> [ Lit "return "; Expr e ]
  | Assign (x, e) -> [ Str x; Lit " := "; Expr e ]
  | AssignCall (x, fe, es) ->
    let call_tkns = tokens_call (Expr fe) (exprs_to_tokens es) in
    Str x :: Lit " := " :: call_tkns
  | AssignECall (x, fn, es) ->
    let call_tkns = tokens_call (Str fn) (exprs_to_tokens es) in
    Str x :: Lit " := " :: Lit "extern " :: call_tkns
  | AssignNewObj x -> [ Str x; Lit " := "; Lit "{ }" ]
  | AssignObjToList (x, e) ->
    Str x :: Lit " := " :: tokens_unopt Operator.ObjectToList (Expr e)
  | AssignObjFields (x, e) ->
    Str x :: Lit " := " :: tokens_unopt Operator.ObjectFields (Expr e)
  | AssignInObjCheck (x, e1, e2) ->
    Str x :: Lit " := " :: tokens_binopt Operator.ObjectMem (Expr e1) (Expr e2)
  | FieldLookup (x, oe, fe) ->
    [ Str x; Lit " := "; Expr oe; Lit "["; Expr fe; Lit "]" ]
  | FieldAssign (oe, fe, e) ->
    [ Expr oe; Lit "["; Expr fe; Lit "]"; Lit " := "; Expr e ]
  | FieldDelete (oe, fe) ->
    [ Lit "delete "; Expr oe; Lit "["; Expr fe; Lit "]" ]
  | If (e, _, _) -> [ Lit "if ("; Expr e; Lit ") { "; Lit threedots ]
  | While (e, _) -> [ Lit "while ("; Expr e; Lit ") { "; Lit threedots ]
  | Fail e -> [ Lit "fail "; Expr e ]
  | Assert e -> [ Lit "assert "; Lit "("; Expr e; Lit ")" ]
  | Abort e -> [ Lit "abort "; Expr e ]

let tokens_func (func : Func.t) : token list =
  let open Func in
  let fn_tkn = Str func.it.name in
  let param_tkns = List.map (fun p -> Str p) func.it.params in
  let end_tkns = [ Lit " { "; Lit threedots ] in
  Lit "function " :: (tokens_call fn_tkn param_tkns @ end_tkns)

let token_region (tkn : token) : Source.region =
  match tkn with
  | Stmt stmt -> stmt.at
  | Func func -> func.at
  | _ -> Source.no_region

let rec token_str (tkn : token) : string =
  match tkn with
  | NoTkn -> ""
  | Index i -> Printf.sprintf "$i%d" i
  | Lit l -> l
  | Str s -> s
  | Val v -> Val.str v
  | Expr expr -> List.map token_str (tokens_expr expr) |> String.concat ""
  | Stmt stmt -> List.map token_str (tokens_stmt stmt) |> String.concat ""
  | Func func -> List.map token_str (tokens_func func) |> String.concat ""

let token_cmp (tkn1 : token) (tkn2 : token) : bool =
  match (tkn1, tkn2) with
  | (Index tkn1', Index tkn2') -> tkn1' = tkn2'
  | (Lit tkn1', Lit tkn2') -> tkn1' = tkn2'
  | (Val tkn1', Val tkn2') -> tkn1' == tkn2'
  | (Str tkn1', Str tkn2') -> tkn1' == tkn2'
  | (Expr tkn1', Expr tkn2') -> tkn1' == tkn2'
  | (Stmt tkn1', Stmt tkn2') -> tkn1' == tkn2'
  | (Func tkn1', Func tkn2') -> tkn1' == tkn2'
  | _ -> false

let token_is_splitable (tkn : token) : bool =
  match tkn with Expr _ | Stmt _ | Func _ -> true | _ -> false

let token_split (tkn : token) : token list =
  match tkn with
  | Expr tkn' -> tokens_expr tkn'
  | Stmt tkn' -> tokens_stmt tkn'
  | Func tkn' -> tokens_func tkn'
  | _ -> []

let token_str_size (tkn : token) : string * int =
  let tkn_str = token_str tkn in
  let cleanTknStr = Font.clean tkn_str in
  (tkn_str, String.length cleanTknStr)

let format_message (error_font : Font.t) (header : string) (msgs : string list)
  : string =
  let fmt_line header msg = Printf.sprintf "\n%s %s" header msg in
  let cause_font = [ error_font; Font.Faint ] in
  let main_header = Font.str_format_err [ error_font ] (header ^ ":") in
  let cause_header = Font.str_format_err cause_font "Caused by:" in
  match msgs with
  | [] -> fmt_line main_header (Font.str_format_err [ error_font ] "???")
  | main_str :: cause_msgs ->
    let fmt_cause_str_f cause_str = fmt_line cause_header cause_str in
    let cause_strs = String.concat "" (List.map fmt_cause_str_f cause_msgs) in
    Printf.sprintf "%s %s%s" main_header main_str cause_strs

type locdata =
  { region : Source.region
  ; mutable file : string
  ; mutable line : int
  ; mutable left : int
  ; mutable right : int
  }

type srcdata =
  { mutable code : string
  ; mutable hgl : string
  ; mutable found : bool
  ; mutable locdata : locdata
  }

let srcdata_init (loc : token) : srcdata =
  let region = token_region loc in
  { code = ""
  ; hgl = ""
  ; found = false
  ; locdata =
      { region
      ; file = region.left.file
      ; line = region.left.line
      ; left = region.left.column
      ; right = region.right.column
      }
  }

let process_empty_source (srcdata : srcdata) (loc : token) : unit =
  let region = token_region loc in
  let (tkn_str, tkn_size) = token_str_size loc in
  srcdata.code <- tkn_str;
  srcdata.hgl <- String.make tkn_size '^';
  srcdata.locdata.left <- region.left.column;
  srcdata.locdata.right <- srcdata.locdata.left + tkn_size - 1

let rec process_existing_source (srcdata : srcdata) (src : token) (loc : token)
  : unit =
  let write_tkn hgl =
    let (tkn_str, tkn_size) = token_str_size loc in
    srcdata.code <- srcdata.code ^ tkn_str;
    srcdata.hgl <- srcdata.hgl ^ String.make tkn_size hgl;
    tkn_size
  in
  if srcdata.found then ignore (write_tkn ' ')
  else if token_cmp loc src then (
    let tkn_size = write_tkn '^' in
    srcdata.locdata.right <- srcdata.locdata.left + tkn_size - 1;
    srcdata.found <- true )
  else if token_is_splitable loc then
    List.iter (process_existing_source srcdata src) (token_split loc)
  else
    let tkn_size = write_tkn ' ' in
    srcdata.locdata.left <- srcdata.locdata.left + tkn_size

let process_source (src : token) (loc : token) : srcdata =
  let srcdata = srcdata_init loc in
  process_existing_source srcdata src loc;
  if not srcdata.found then process_empty_source srcdata loc;
  srcdata

let format_loc ?(colon : bool = true) (locdata : locdata) : string =
  if locdata.region = Source.no_region then ""
  else
    let colon_str = if colon then ":" else "" in
    Printf.sprintf "File \"%s\", line %d, characters %d-%d%s" locdata.file
      locdata.line locdata.left locdata.right colon_str

let format_code (srcdata : srcdata) : string =
  if not !Config.Eslerr.show_code then ""
  else
    let code_header =
      if srcdata.locdata.region = Source.no_region then String.make 6 ' '
      else Printf.sprintf "%d |  " srcdata.locdata.line
    in
    Printf.sprintf "%s%s" code_header srcdata.code

let format_hgl (hgl_font : Font.t) (srcdata : srcdata) : string =
  let lineno_size = String.length (string_of_int srcdata.locdata.line) in
  let err_cause_ident = String.make (lineno_size + 5) ' ' in
  Font.str_format_err [ hgl_font ] (err_cause_ident ^ srcdata.hgl)

let format_source (error_font : Font.t) (loc : token) (src : token) : string =
  let loc_font = [ Font.Italic; Font.Faint ] in
  match (!Config.Eslerr.show_code, loc) with
  | (_, NoTkn) -> ""
  | (false, _) ->
    let colon = false in
    let locdata = (srcdata_init loc).locdata in
    let loc_str = Font.str_format_err loc_font (format_loc ~colon locdata) in
    Printf.sprintf "%s" loc_str
  | (true, _) ->
    let srcdata = process_source src loc in
    let loc_str = Font.str_format_err loc_font (format_loc srcdata.locdata) in
    let code_str = format_code srcdata in
    let hgl_str = format_hgl error_font srcdata in
    Printf.sprintf "%s\n%s\n%s" loc_str code_str hgl_str
