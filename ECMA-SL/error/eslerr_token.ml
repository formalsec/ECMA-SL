type t =
  | NoTkn
  | Index of int
  | Lit of string
  | Str of string
  | Val of Val.t
  | Expr of Expr.t
  | Stmt of Stmt.t
  | Func of Func.t

let not_implemented () : 'a = failwith "not implemented in eslerr" [@@inline]
let threedots () = Lit (Font.str_text_err [ Font.Faint ] "...") [@@inline]

let separate (sep : string) (tkns : t list) : t list =
  let sep_f tkn = function [] -> [ tkn ] | acc -> tkn :: Lit sep :: acc in
  List.fold_right sep_f tkns []

let from_exprs (es : Expr.t list) : t list = List.map (fun e -> Expr e) es

let from_call (call_tkn : t) (args_tkn : t list) : t list =
  let argTkns = separate ", " args_tkn in
  call_tkn :: Lit "(" :: (argTkns @ [ Lit ")" ])

let from_unopt (op : Operator.unopt) (e_tkn : t) : t list =
  let op_tkn = Lit (Operator.str_of_unopt_single op) in
  let args_tkn () = [ e_tkn ] in
  if Operator.is_infix_unopt op then [ op_tkn; e_tkn ]
  else from_call op_tkn (args_tkn ())

let from_binopt (op : Operator.binopt) (e1_tkn : t) (e2_tkn : t) : t list =
  let op_tkn = Lit (Operator.str_of_binopt_single op) in
  let args_tkn () = [ e1_tkn; e2_tkn ] in
  if Operator.is_infix_binopt op then [ e1_tkn; op_tkn; e2_tkn ]
  else from_call op_tkn (args_tkn ())

let from_triopt (op : Operator.triopt) (e1_tkn : t) (e2_tkn : t) (e3_tkn : t) :
  t list =
  let op_tkn = Lit (Operator.str_of_triopt_single op) in
  let args_tkn () = [ e1_tkn; e2_tkn; e3_tkn ] in
  from_call op_tkn (args_tkn ())

let from_nopt (op : Operator.nopt) (es_tkn : t list) : t list =
  let sep_tkns sep = separate sep es_tkn in
  match op with
  | NAryLogicalAnd -> sep_tkns " && "
  | NAryLogicalOr -> sep_tkns " || "
  | ArrayExpr -> List.concat [ [ Lit "[|" ]; sep_tkns ", "; [ Lit "|]" ] ]
  | ListExpr -> List.concat [ [ Lit "[" ]; sep_tkns ", "; [ Lit "]" ] ]
  | TupleExpr -> List.concat [ [ Lit "(" ]; sep_tkns ", "; [ Lit ")" ] ]

let from_expr (e : Expr.t) : t list =
  match e with
  | Val v -> [ Val v ]
  | Var x -> [ Str x ]
  | UnOpt (op, e') -> from_unopt op (Expr e')
  | BinOpt (op, e1, e2) -> from_binopt op (Expr e1) (Expr e2)
  | TriOpt (op, e1, e2, e3) -> from_triopt op (Expr e1) (Expr e2) (Expr e3)
  | NOpt (op, es) -> from_nopt op (from_exprs es)
  | Curry _ -> not_implemented ()
  | Symbolic _ -> not_implemented ()

let from_stmt (s : Stmt.t) : t list =
  match s.it with
  | Skip -> []
  | Merge -> []
  | Debug s' -> [ Stmt s' ]
  | Block _ -> []
  | Print e -> [ Lit "print "; Expr e ]
  | Return e -> [ Lit "return "; Expr e ]
  | Assign (x, e) -> [ Str x.it; Lit " := "; Expr e ]
  | AssignCall (x, fe, es) ->
    Str x.it :: Lit " := " :: from_call (Expr fe) (from_exprs es)
  | AssignECall (x, fn, es) ->
    Str x.it
    :: Lit " := "
    :: Lit "extern "
    :: from_call (Str fn.it) (from_exprs es)
  | AssignNewObj x -> [ Str x.it; Lit " := "; Lit "{ }" ]
  | AssignObjToList (x, e) ->
    Str x.it :: Lit " := " :: from_unopt Operator.ObjectToList (Expr e)
  | AssignObjFields (x, e) ->
    Str x.it :: Lit " := " :: from_unopt Operator.ObjectFields (Expr e)
  | AssignInObjCheck (x, e1, e2) ->
    Str x.it :: Lit " := " :: from_binopt Operator.ObjectMem (Expr e1) (Expr e2)
  | FieldLookup (x, oe, fe) ->
    [ Str x.it; Lit " := "; Expr oe; Lit "["; Expr fe; Lit "]" ]
  | FieldAssign (oe, fe, e) ->
    [ Expr oe; Lit "["; Expr fe; Lit "]"; Lit " := "; Expr e ]
  | FieldDelete (oe, fe) ->
    [ Lit "delete "; Expr oe; Lit "["; Expr fe; Lit "]" ]
  | If (e, _, _) -> [ Lit "if ("; Expr e; Lit ") { "; threedots () ]
  | While (e, _) -> [ Lit "while ("; Expr e; Lit ") { "; threedots () ]
  | Fail e -> [ Lit "fail "; Expr e ]
  | Assert e -> [ Lit "assert "; Lit "("; Expr e; Lit ")" ]

let from_func (f : Func.t) : t list =
  let fn_tkn = Str (Func.name' f) in
  let param_tkns = List.map (fun p -> Str p) (Func.params' f) in
  let end_tkns = [ Lit " { "; threedots () ] in
  Lit "function " :: (from_call fn_tkn param_tkns @ end_tkns)

let region (tkn : t) : Source.region =
  match tkn with Stmt s -> s.at | Func f -> f.at | _ -> Source.no_region

let rec pp (fmt : Fmt.t) (tkn : t) : unit =
  let open Fmt in
  match tkn with
  | NoTkn -> ()
  | Index i -> fprintf fmt "$i%d" i
  | Lit l -> fprintf fmt "%s" l
  | Str s -> fprintf fmt "%s" s
  | Val v -> fprintf fmt "%a" Val.pp v
  | Expr e -> fprintf fmt "%a" (pp_lst "" pp) (from_expr e)
  | Stmt s -> fprintf fmt "%a" (pp_lst "" pp) (from_stmt s)
  | Func f -> fprintf fmt "%a" (pp_lst "" pp) (from_func f)

let str (tkn : t) : string = Fmt.asprintf "%a" pp tkn

let str_size (tkn : t) : string * int =
  let tkn_str = str tkn in
  let tkn_str_clean = Font.clean tkn_str in
  (tkn_str, String.length tkn_str_clean)

let cmp (tkn1 : t) (tkn2 : t) : bool =
  match (tkn1, tkn2) with
  | (Index tkn1', Index tkn2') -> tkn1' = tkn2'
  | (Lit tkn1', Lit tkn2') -> tkn1' = tkn2'
  | (Val tkn1', Val tkn2') -> tkn1' == tkn2'
  | (Str tkn1', Str tkn2') -> tkn1' == tkn2'
  | (Expr tkn1', Expr tkn2') -> tkn1' == tkn2'
  | (Stmt tkn1', Stmt tkn2') -> tkn1' == tkn2'
  | (Func tkn1', Func tkn2') -> tkn1' == tkn2'
  | _ -> false

let is_splitable (tkn : t) : bool =
  match tkn with Expr _ | Stmt _ | Func _ -> true | _ -> false

let split (tkn : t) : t list =
  match tkn with
  | Expr tkn' -> from_expr tkn'
  | Stmt tkn' -> from_stmt tkn'
  | Func tkn' -> from_func tkn'
  | _ -> []
