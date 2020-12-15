(* parser-specification file *)

(*
  BEGIN first section - declarations
  - token and type specifications, precedence directives and other output directives
*)
%token SKIP
%token PRINT
%token DEFEQ
%token WHILE
%token IF ELSE
%token RETURN
%token NULL
%token FUNCTION
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token PERIOD COMMA SEMICOLON
%token DELETE
%token FAIL
%token THROW
%token <float> FLOAT
%token <int> INT
%token <bool> BOOLEAN
%token <string> VAR
%token <string> STRING
%token <string> SYMBOL
%token <string> LOC
%token LAND LOR
%token INT_TO_FLOAT INT_TO_STRING INT_OF_STRING FLOAT_OF_STRING FLOAT_TO_STRING OBJ_TO_LIST OBJ_FIELDS INT_OF_FLOAT
%token BITWISE_NOT BITWISE_AND BITWISE_OR BITWISE_XOR SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%token FROM_CHAR_CODE TO_CHAR_CODE TO_LOWER_CASE TO_UPPER_CASE TRIM
%token TO_INT TO_INT32 TO_UINT32 TO_UINT16
%token ABS ACOS ASIN ATAN ATAN_2 CEIL COS EXP FLOOR LOG_E LOG_10 MAX MIN POW RANDOM ROUND SIN SQRT TAN
%token PLUS MINUS TIMES DIVIDE MODULO EQUAL GT LT EGT ELT IN_OBJ IN_LIST
%token NOT LLEN LNTH LADD LPREPEND LCONCAT HD TL TLEN TNTH FST SND SLEN SNTH
%token SCONCAT AT_SIGN
%token TYPEOF INT_TYPE FLT_TYPE BOOL_TYPE STR_TYPE LOC_TYPE
%token LIST_TYPE TUPLE_TYPE NULL_TYPE SYMBOL_TYPE CURRY_TYPE
%token EOF

%left LAND LOR BITWISE_AND BITWISE_OR BITWISE_XOR SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL POW
%left GT LT EGT ELT IN_LIST
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left EQUAL


%nonassoc binopt_prec
%nonassoc unopt_prec


%type <Func.t > proc_target
%type <Func.t list> prog_target

%start prog_target
%% (* separator line *)
(* END first section - declarations *)

(*
  BEGIN - second section - grammar and rules
  - specifying the grammar of the language to be parsed, specifying the productions
  - productions are organized into rules, where each rule lists all
    the possible productions for a given nonterminal.
*)
(*
prog_expr_target:
  | e = expr_target; EOF; { e }

prog_stmt_target:
  | s = stmt_target; EOF; { s }
*)
prog_target:
  | funcs = separated_list (SEMICOLON, proc_target); EOF;
   { funcs }

proc_target:
  | FUNCTION; f = VAR; LPAREN; vars = separated_list (COMMA, VAR); RPAREN; LBRACE; s = stmt_block; RBRACE
   { Func.create f vars s }

(*
  The pipes separate the individual productions, and the curly braces contain a semantic action:
    OCaml code that generates the OCaml value corresponding to the production in question.
  Semantic actions are arbitrary OCaml expressions that are evaluated during parsing
    to produce values that are attached to the nonterminal in the rule.
*)

tuple_target:
  | v1 = expr_target; COMMA; v2 = expr_target;
    { [v2; v1] }
  | vs = tuple_target; COMMA; v = expr_target;
    { v :: vs }

type_target:
  | INT_TYPE;
    { print_string ">INT_TYPE\n"; Type.IntType }
  | FLT_TYPE;
    { print_string ">FLT_TYPE\n"; Type.FltType }
  | BOOL_TYPE;
    { print_string ">BOOL_TYPE\n"; Type.BoolType }
  | STR_TYPE;
    { print_string ">STR_TYPE\n"; Type.StrType }
  | LOC_TYPE;
    { print_string ">LOC_TYPE\n"; Type.LocType }
  | LIST_TYPE;
    { print_string ">LIST_TYPE\n"; Type.ListType }
  | TUPLE_TYPE;
    { print_string ">TUPLE_TYPE\n"; Type.TupleType }
  | NULL_TYPE;
    { print_string ">NULL_TYPE\n"; Type.NullType }
  | SYMBOL_TYPE;
    { print_string ">SYMBOL_TYPE\n"; Type.SymbolType }
  | CURRY_TYPE;
    { Type.CurryType }

(* v ::= f | i | b | s *)
val_target:
  | NULL;
    { print_string ">NULL\n";Val.Null }
  | f = FLOAT;
    { print_string ">FLOAT\n";Val.Flt f }
  | i = INT;
    { print_string ">INT\n";Val.Int i }
  | b = BOOLEAN;
    { print_string ">BOOL\n";Val.Bool b }
  | s = STRING;
    { print_string ">STR\n";Val.Str s }
  | s = SYMBOL;
    { print_string ">SYMBOL\n";Val.Symbol s }
  | l = LOC;
    { Val.Loc l }
  | t = type_target;
    { print_string ">TYPE \n";Val.Type t }

(* e ::= {} | {f:e} | [] | [e] | e.f | e[f] | v | x | -e | e+e | f(e) | (e) *)
expr_target:
  | LBRACK; es = separated_list (COMMA, expr_target); RBRACK;
    { print_string ">NOP\n";Expr.NOpt (Oper.ListExpr, es) }
  | LPAREN; t = tuple_target; RPAREN;
    { Expr.NOpt (Oper.TupleExpr, List.rev t) }
  | v = val_target;
    { print_string ">VAL\n"; Expr.Val v }
  | v = VAR;
    { print_string ">VAR\n";  Expr.Var v }
  | LBRACE; e = expr_target; RBRACE; AT_SIGN; LPAREN; es = separated_list (COMMA, expr_target); RPAREN;
    { Expr.Curry (e, es) }
  | MINUS; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.Neg, e) } %prec unopt_prec
  | NOT; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.Not, e) } %prec unopt_prec
  | BITWISE_NOT; e = expr_target;
    {  print_string ">UNOP\n"; Expr.UnOpt (Oper.BitwiseNot, e) } %prec unopt_prec
  | LLEN; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.ListLen, e) } %prec unopt_prec
  | TLEN; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.TupleLen, e) } %prec unopt_prec
  | SLEN; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.StringLen, e) } %prec unopt_prec
  | TYPEOF; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.Typeof, e) } %prec unopt_prec
  | HD; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.Head, e) } %prec unopt_prec
  | TL; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.Tail, e) } %prec unopt_prec
  | FST; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.First, e) } %prec unopt_prec
  | SND; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.Second, e) } %prec unopt_prec
  | INT_TO_FLOAT; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.IntToFloat, e) } %prec unopt_prec
  | INT_TO_STRING; e = expr_target;
    { Expr.UnOpt (Oper.IntToString, e) } %prec unopt_prec
  | INT_OF_STRING; e = expr_target;
    { Expr.UnOpt (Oper.IntOfString, e) } %prec unopt_prec
  | INT_OF_FLOAT; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.IntOfFloat, e) } %prec unopt_prec
  | TO_INT; e = expr_target;
    { Expr.UnOpt (Oper.ToInt, e) } %prec unopt_prec
  | TO_INT32; e = expr_target;
    { Expr.UnOpt (Oper.ToInt32, e) } %prec unopt_prec
  | TO_UINT32; e = expr_target;
    { Expr.UnOpt (Oper.ToUint32, e) } %prec unopt_prec
  | FROM_CHAR_CODE; e = expr_target;
    { Expr.UnOpt (Oper.FromCharCode, e) } %prec unopt_prec
  | TO_CHAR_CODE; e = expr_target;
    { Expr.UnOpt (Oper.ToCharCode, e) } %prec unopt_prec
  | TO_LOWER_CASE; e = expr_target;
    { Expr.UnOpt (Oper.ToLowerCase, e) } %prec unopt_prec
  | TO_UPPER_CASE; e = expr_target;
    { Expr.UnOpt (Oper.ToUpperCase, e) } %prec unopt_prec
  | TRIM; e = expr_target;
    { Expr.UnOpt (Oper.Trim, e) } %prec unopt_prec
  | TO_UINT16; e = expr_target;
    { Expr.UnOpt (Oper.ToUint16, e) } %prec unopt_prec
  | ABS; e = expr_target;
    { Expr.UnOpt (Oper.Abs, e) } %prec unopt_prec
  | ACOS; e = expr_target;
    { Expr.UnOpt (Oper.Acos, e) } %prec unopt_prec
  | ASIN; e = expr_target;
    { Expr.UnOpt (Oper.Asin, e) } %prec unopt_prec
  | ATAN; e = expr_target;
    { Expr.UnOpt (Oper.Atan, e) } %prec unopt_prec
  | CEIL; e = expr_target;
    { Expr.UnOpt (Oper.Ceil, e) } %prec unopt_prec
  | COS; e = expr_target;
    { Expr.UnOpt (Oper.Cos, e) } %prec unopt_prec
  | EXP; e = expr_target;
    { Expr.UnOpt (Oper.Exp, e) } %prec unopt_prec
  | FLOOR; e = expr_target;
    { Expr.UnOpt (Oper.Floor, e) } %prec unopt_prec
  | LOG_E; e = expr_target;
    { Expr.UnOpt (Oper.Log_e, e) } %prec unopt_prec
  | LOG_10; e = expr_target;
    { Expr.UnOpt (Oper.Log_10, e) } %prec unopt_prec
  | ROUND; e = expr_target;
    { Expr.UnOpt (Oper.Round, e) } %prec unopt_prec
  | RANDOM; e = expr_target;
    { Expr.UnOpt (Oper.Random, e) } %prec unopt_prec
  | SIN; e = expr_target;
    { Expr.UnOpt (Oper.Sin, e) } %prec unopt_prec
  | SQRT; e = expr_target;
    { Expr.UnOpt (Oper.Sqrt, e) } %prec unopt_prec
  | TAN; e = expr_target;
    { Expr.UnOpt (Oper.Tan, e) } %prec unopt_prec
  | FLOAT_TO_STRING; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.FloatToString, e) } %prec unopt_prec
  | FLOAT_OF_STRING; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.FloatOfString, e) } %prec unopt_prec
  | e1 = expr_target; bop = op_target; e2 = expr_target;
    { print_string ">BINOP\n";Expr.BinOpt (bop, e1, e2) } %prec binopt_prec
  | LNTH; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { print_string ">BINOP\n";Expr.BinOpt (Oper.Lnth, e1, e2) }
  | TNTH; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { print_string ">BINOP\n";Expr.BinOpt (Oper.Tnth, e1, e2) }
  | SNTH; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { print_string ">BINOP\n";Expr.BinOpt (Oper.Snth, e1, e2) }
  | LADD; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { print_string ">BINOP\n";Expr.BinOpt (Oper.Ladd, e1, e2) }
  | LPREPEND; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Lprepend, e1, e2) }
  | LCONCAT; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { print_string ">BINOP\n";Expr.BinOpt (Oper.Lconcat, e1, e2) }
  | LPAREN; e = expr_target; RPAREN;
    { print_string ">PAREN\n";e }
  | SCONCAT; e = expr_target;
    { print_string ">UNOP\n";Expr.UnOpt (Oper.Sconcat, e) } %prec unopt_prec
  | ATAN_2; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Atan2, e1, e2) }
  | MAX; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Max, e1, e2) }
  | MIN; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Min, e1, e2) }

stmt_block:
  | s = separated_list (SEMICOLON, stmt_target);
    { print_string ">BLOCK\n";Stmt.Block s }

(* s ::= e.f := e | delete e.f | skip | x := e | s1; s2 | if (e) { s1 } else { s2 } | while (e) { s } | return e | return *)
stmt_target:
  | PRINT; e = expr_target;
    { Stmt.Print e }
  | FAIL; e = expr_target;
    { Stmt.Fail e }
  | THROW; str = STRING;
    { Stmt.Exception str}
  | e1 = expr_target; PERIOD; f = VAR; DEFEQ; e2 = expr_target;
    { print_string ">FIELDASSIGN\n"; Stmt.FieldAssign (e1, Expr.Val (Str f), e2) }
  | e1 = expr_target; LBRACK; f = expr_target; RBRACK; DEFEQ; e2 = expr_target;
    { print_string ">FIELDASSIGN\n"; Stmt.FieldAssign (e1, f, e2) }
  | DELETE; e = expr_target; PERIOD; f = VAR;
    { print_string ">FIELDDELETE\n"; Stmt.FieldDelete (e, Expr.Val (Str f)) }
  | DELETE; e = expr_target; LBRACK; f = expr_target; RBRACK;
    { print_string ">FIELDDELETE\n"; Stmt.FieldDelete (e, f) }
  | SKIP;
    { print_string ">SKIP\n";Stmt.Skip }
  | v = VAR; DEFEQ; e = expr_target;
    { print_string ">ASSIGN\n"; Stmt.Assign (v, e) }
  | exps_stmts = ifelse_target;
    { exps_stmts }
  | WHILE; LPAREN; e = expr_target; RPAREN; LBRACE; s = stmt_block; RBRACE;
    { print_string ">WHILE\n"; Stmt.While (e, s) }
  | RETURN; e = expr_target;
    { print_string ">RETURN\n"; Stmt.Return e }
  | RETURN;
    { print_string ">RETURN\n";Stmt.Return (Expr.Val Val.Void) }
  | v = VAR; DEFEQ; f = expr_target; LPAREN; vs = separated_list(COMMA, expr_target); RPAREN;
    { print_string ">ASSIGNCALL\n";Stmt.AssignCall (v,f,vs)}
  | v = VAR; DEFEQ; e1 = expr_target; IN_OBJ; e2 = expr_target;
    { print_string ">ASSIGNINOBJCHECK\n";Stmt.AssignInObjCheck (v,e1,e2)}
  | v = VAR; DEFEQ; e = expr_target; PERIOD; f = VAR;
    { print_string ">ASSIGNACCESS\n";Stmt.FieldLookup (v,e, Expr.Val (Str f)) }
  | v = VAR; DEFEQ; e = expr_target; LBRACK; f = expr_target; RBRACK;
    { print_string ">ASSIGNACCESS\n";Stmt.FieldLookup (v,e, f) }
  | v = VAR; DEFEQ; LBRACE; RBRACE;
    { print_string ">ASSIGNNEWOBJ\n";Stmt.AssignNewObj (v) }
  | v = VAR; DEFEQ; OBJ_TO_LIST; e = expr_target;
    { print_endline ">ASSIGNOBJTOLIST"; Stmt.AssignObjToList (v, e) }
  | v = VAR; DEFEQ; OBJ_FIELDS; e = expr_target;
    { print_endline ">ASSIGNOBJFIELDS"; Stmt.AssignObjFields (v, e) }



(* if (e) { s } | if (e) {s} else { s } *)
ifelse_target:
  | IF; LPAREN; e = expr_target; RPAREN; LBRACE; s1 = stmt_block; RBRACE; ELSE;LBRACE; s2 = stmt_block; RBRACE;
    { print_string ">IF\n";Stmt.If(e, s1, Some s2) }
  | IF; LPAREN; e = expr_target; RPAREN; LBRACE; s = stmt_block; RBRACE;
    { print_string ">IF\n";Stmt.If(e, s, None) }

op_target:
  | MINUS   { Oper.Minus }
  | PLUS    { Oper.Plus }
  | TIMES   { Oper.Times }
  | DIVIDE  { Oper.Div }
  | MODULO  { Oper.Modulo }
  | EQUAL   { Oper.Equal }
  | GT      { Oper.Gt }
  | LT      { Oper.Lt }
  | EGT     { Oper.Egt }
  | ELT     { Oper.Elt }
  | LAND    { Oper.Log_And }
  | BITWISE_AND { Oper.BitwiseAnd }
  | BITWISE_OR { Oper.BitwiseOr }
  | BITWISE_XOR { Oper.BitwiseXor }
  | SHIFT_LEFT { Oper.ShiftLeft }
  | SHIFT_RIGHT { Oper.ShiftRight }
  | SHIFT_RIGHT_LOGICAL { Oper.ShiftRightLogical }
  | LOR     { Oper.Log_Or }
  | IN_LIST { Oper.InList }
  | POW     { Oper.Pow }
