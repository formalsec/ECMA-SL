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
%token LARRBRACK RARRBRACK
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token PERIOD COMMA SEMICOLON
%token DELETE
%token FAIL
%token ABORT
%token ASSUME ASSERT SYMBOLIC
%token THROW
%token <float> FLOAT
%token <int> INT
%token <bool> BOOLEAN
%token <string> VAR
%token <string> STRING
%token <string> SYMBOL
%token <string> LOC
%token LAND LOR
%token PARSE_NUMBER PARSE_STRING PARSE_DATE INT_TO_FLOAT INT_TO_STRING INT_TO_FOUR_HEX HEX_DECODE UTF8_DECODE OCTAL_TO_DECIMAL
%token INT_OF_STRING FLOAT_OF_STRING FLOAT_TO_STRING OBJ_TO_LIST OBJ_FIELDS INT_OF_FLOAT
%token BITWISE_NOT BITWISE_AND BITWISE_OR BITWISE_XOR SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%token FROM_CHAR_CODE FROM_CHAR_CODE_U TO_CHAR_CODE TO_CHAR_CODE_U TO_LOWER_CASE TO_UPPER_CASE TRIM
%token TO_INT TO_INT32 TO_UINT32 TO_UINT16
%token ABS ACOS ASIN ATAN ATAN_2 CEIL COS EXP FLOOR LOG_E LOG_10 MAX MIN POW RANDOM SIN SQRT TAN COSH LOG_2 SINH TANH FLOAT64_TO_LE_BYTES FLOAT64_TO_BE_BYTES FLOAT32_TO_LE_BYTES FLOAT32_TO_BE_BYTES INT_TO_BE_BYTES FLOAT64_FROM_LE_BYTES FLOAT64_FROM_BE_BYTES FLOAT32_FROM_LE_BYTES FLOAT32_FROM_BE_BYTES INT_FROM_BYTES UINT_FROM_BYTES BYTES_TO_STRING FLOAT_TO_BYTE IS_NAN
%token PLUS MINUS TIMES DIVIDE MODULO EQUAL GT LT EGT ELT IN_OBJ IN_LIST TO_PRECISION TO_EXPONENTIAL TO_FIXED
%token NOT LLEN LNTH LREMNTH LREM LADD LSET LPREPEND LCONCAT LREVERSE LREMOVELAST LSORT HD TL TLEN TNTH FST SND SLEN SLEN_U SNTH SNTH_U SSUBSTR SSUBSTR_U ARRAY_MAKE ANTH ASET ALEN LIST_TO_ARRAY
%token SCONCAT SSPLIT AT_SIGN EXTERN
%token TYPEOF INT_TYPE FLT_TYPE BOOL_TYPE STR_TYPE LOC_TYPE
%token LIST_TYPE TUPLE_TYPE NULL_TYPE SYMBOL_TYPE CURRY_TYPE
%token EOF

%left LAND LOR
%left EQUAL
%left GT LT EGT ELT IN_LIST BITWISE_AND BITWISE_OR BITWISE_XOR SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right POW

%nonassoc unopt_prec


%type <Func.t > proc_target
%type <Func.t list> prog_target

%start prog_target proc_target
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
    { Type.IntType }
  | FLT_TYPE;
    { Type.FltType }
  | BOOL_TYPE;
    { Type.BoolType }
  | STR_TYPE;
    { Type.StrType }
  | LOC_TYPE;
    { Type.LocType }
  | LIST_TYPE;
    { Type.ListType }
  | TUPLE_TYPE;
    { Type.TupleType }
  | NULL_TYPE;
    { Type.NullType }
  | SYMBOL_TYPE;
    { Type.SymbolType }
  | CURRY_TYPE;
    { Type.CurryType }

(* v ::= f | i | b | s *)
val_target:
  | NULL;
    { Val.Null }
  | f = FLOAT;
    { Val.Flt f }
  | i = INT;
    { Val.Int i }
  | b = BOOLEAN;
    { Val.Bool b }
  | s = STRING;
    { Val.Str s }
  | s = SYMBOL;
    { Val.Symbol s }
  | l = LOC;
    { Val.Loc l }
  | t = type_target;
    { Val.Type t }

(* e ::= {} | {f:e} | [] | [e] | e.f | e[f] | v | x | -e | e+e | f(e) | (e) *)
expr_target:
  | LBRACK; es = separated_list (COMMA, expr_target); RBRACK;
    { Expr.NOpt (Oper.ListExpr, es) }
  | LARRBRACK; es = separated_list (COMMA, expr_target); RARRBRACK;
    { Expr.NOpt (Oper.ArrExpr, es) }
  | LPAREN; t = tuple_target; RPAREN;
    { Expr.NOpt (Oper.TupleExpr, List.rev t) }
  | v = val_target;
    { Expr.Val v }
  | v = VAR;
    { Expr.Var v }
  | SYMBOLIC; LPAREN; t = type_target; COMMA; x = STRING; RPAREN;
    { Expr.Symbolic (t, x) }
  | LBRACE; e = expr_target; RBRACE; AT_SIGN; LPAREN; es = separated_list (COMMA, expr_target); RPAREN;
    { Expr.Curry (e, es) }
  | MINUS; e = expr_target;
    { Expr.UnOpt (Oper.Neg, e) } %prec unopt_prec
  | NOT; e = expr_target;
    { Expr.UnOpt (Oper.Not, e) } %prec unopt_prec
  | IS_NAN; e = expr_target;
    { Expr.UnOpt (Oper.IsNaN, e) } %prec unopt_prec
  | BITWISE_NOT; e = expr_target;
    { Expr.UnOpt (Oper.BitwiseNot, e) } %prec unopt_prec
  | LLEN; e = expr_target;
    { Expr.UnOpt (Oper.ListLen, e) } %prec unopt_prec
  | TLEN; e = expr_target;
    { Expr.UnOpt (Oper.TupleLen, e) } %prec unopt_prec
  | SLEN; e = expr_target;
    { Expr.UnOpt (Oper.StringLen, e) } %prec unopt_prec
  | SLEN_U; e = expr_target;
    { Expr.UnOpt (Oper.StringLenU, e) } %prec unopt_prec
  | TYPEOF; e = expr_target;
    { Expr.UnOpt (Oper.Typeof, e) } %prec unopt_prec
  | HD; e = expr_target;
    { Expr.UnOpt (Oper.Head, e) } %prec unopt_prec
  | TL; e = expr_target;
    { Expr.UnOpt (Oper.Tail, e) } %prec unopt_prec
  | FST; e = expr_target;
    { Expr.UnOpt (Oper.First, e) } %prec unopt_prec
  | SND; e = expr_target;
    { Expr.UnOpt (Oper.Second, e) } %prec unopt_prec
  | LREMOVELAST; e = expr_target;
    { Expr.UnOpt (Oper.LRemoveLast, e) } %prec unopt_prec
  | LSORT; e = expr_target;
    { Expr.UnOpt (Oper.LSort, e) } %prec unopt_prec
  | INT_TO_FLOAT; e = expr_target;
    { Expr.UnOpt (Oper.IntToFloat, e) } %prec unopt_prec
  | INT_TO_STRING; e = expr_target;
    { Expr.UnOpt (Oper.IntToString, e) } %prec unopt_prec
  | INT_TO_FOUR_HEX; e = expr_target;
    { Expr.UnOpt (Oper.IntToFourHex, e) } %prec unopt_prec
  | HEX_DECODE; e = expr_target;
    { Expr.UnOpt (Oper.HexDecode, e) } %prec unopt_prec
  | UTF8_DECODE; e = expr_target;
    { Expr.UnOpt (Oper.Utf8Decode, e) } %prec unopt_prec
  | OCTAL_TO_DECIMAL; e = expr_target;
    { Expr.UnOpt (Oper.OctalToDecimal, e) } %prec unopt_prec
  | INT_OF_STRING; e = expr_target;
    { Expr.UnOpt (Oper.IntOfString, e) } %prec unopt_prec
  | INT_OF_FLOAT; e = expr_target;
    { Expr.UnOpt (Oper.IntOfFloat, e) } %prec unopt_prec
  | TO_INT; e = expr_target;
    { Expr.UnOpt (Oper.ToInt, e) } %prec unopt_prec
  | TO_INT32; e = expr_target;
    { Expr.UnOpt (Oper.ToInt32, e) } %prec unopt_prec
  | TO_UINT32; e = expr_target;
    { Expr.UnOpt (Oper.ToUint32, e) } %prec unopt_prec
  | FROM_CHAR_CODE; e = expr_target;
    { Expr.UnOpt (Oper.FromCharCode, e) } %prec unopt_prec
  | FROM_CHAR_CODE_U; e = expr_target;
    { Expr.UnOpt (Oper.FromCharCodeU, e) } %prec unopt_prec
  | TO_CHAR_CODE; e = expr_target;
    { Expr.UnOpt (Oper.ToCharCode, e) } %prec unopt_prec
  | TO_CHAR_CODE_U; e = expr_target;
    { Expr.UnOpt (Oper.ToCharCodeU, e) } %prec unopt_prec
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
  | RANDOM; e = expr_target;
    { Expr.UnOpt (Oper.Random, e) } %prec unopt_prec
  | SIN; e = expr_target;
    { Expr.UnOpt (Oper.Sin, e) } %prec unopt_prec
  | SQRT; e = expr_target;
    { Expr.UnOpt (Oper.Sqrt, e) } %prec unopt_prec
  | TAN; e = expr_target;
    { Expr.UnOpt (Oper.Tan, e) } %prec unopt_prec
  | FLOAT_TO_STRING; e = expr_target;
    { Expr.UnOpt (Oper.FloatToString, e) } %prec unopt_prec
  | FLOAT_OF_STRING; e = expr_target;
    { Expr.UnOpt (Oper.FloatOfString, e) } %prec unopt_prec
  | e1 = expr_target; bop = op_target; e2 = expr_target;
    { Expr.BinOpt (bop, e1, e2) }
  | LNTH; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Lnth, e1, e2) }
  | LREM; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.LRem, e1, e2) }
  | LREMNTH; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.LRemNth, e1, e2) }
  | TNTH; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Tnth, e1, e2) }
  | SNTH; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Snth, e1, e2) }
  | SNTH_U; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Snth_u, e1, e2) }
  | SSPLIT; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Ssplit, e1, e2) }
  | SSUBSTR; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; COMMA; e3 = expr_target; RPAREN;
    { Expr.TriOpt (Oper.Ssubstr, e1, e2, e3) }
  | SSUBSTR_U; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; COMMA; e3 = expr_target; RPAREN;
    { Expr.TriOpt (Oper.SsubstrU, e1, e2, e3) }
  | LADD; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Ladd, e1, e2) }
  | LPREPEND; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Lprepend, e1, e2) }
  | LCONCAT; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Lconcat, e1, e2) }
  | LREVERSE; e = expr_target;
    { Expr.UnOpt (Oper.LReverse, e) } %prec unopt_prec
  | LPAREN; e = expr_target; RPAREN;
    { e }
  | SCONCAT; e = expr_target;
    { Expr.UnOpt (Oper.Sconcat, e) } %prec unopt_prec
  | ATAN_2; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Atan2, e1, e2) }
  | MAX; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Max, e1, e2) }
  | MIN; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Min, e1, e2) }
  | PARSE_NUMBER; e = expr_target;
    { Expr.UnOpt (Oper.ParseNumber, e) } %prec unopt_prec
  | PARSE_STRING; e = expr_target;
    { Expr.UnOpt (Oper.ParseString, e) } %prec unopt_prec
  | PARSE_DATE; e = expr_target;
    { Expr.UnOpt (Oper.ParseDate, e) } %prec unopt_prec
  | TO_PRECISION; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN; 
    { Expr.BinOpt (Oper.ToPrecision, e1, e2) }
  | TO_EXPONENTIAL; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN; 
    { Expr.BinOpt (Oper.ToExponential, e1, e2) }
  | TO_FIXED; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN; 
    { Expr.BinOpt (Oper.ToFixed, e1, e2) }
  | COSH;  e = expr_target;
    { Expr.UnOpt (Oper.Cosh, e) } %prec unopt_prec 
  | LOG_2;  e = expr_target;
    { Expr.UnOpt (Oper.Log_2, e) } %prec unopt_prec 
  | SINH;  e = expr_target;
    { Expr.UnOpt (Oper.Sinh, e) } %prec unopt_prec 
  | TANH;  e = expr_target;
    { Expr.UnOpt (Oper.Tanh, e) } %prec unopt_prec
  | FLOAT64_TO_LE_BYTES; e = expr_target;
    { Expr.UnOpt (Oper.Float64ToLEBytes, e) } %prec unopt_prec 
  | FLOAT64_TO_BE_BYTES; e = expr_target;
    { Expr.UnOpt (Oper.Float64ToBEBytes, e) } %prec unopt_prec 
  | FLOAT32_TO_LE_BYTES; e = expr_target;
    { Expr.UnOpt (Oper.Float32ToLEBytes, e) } %prec unopt_prec 
  | FLOAT32_TO_BE_BYTES; e = expr_target;
    { Expr.UnOpt (Oper.Float32ToBEBytes, e) } %prec unopt_prec 
  | INT_TO_BE_BYTES; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.IntToBEBytes, e1, e2) }
  | FLOAT64_FROM_LE_BYTES; e = expr_target;
    { Expr.UnOpt (Oper.Float64FromLEBytes, e) } %prec unopt_prec 
  | FLOAT64_FROM_BE_BYTES; e = expr_target;
    { Expr.UnOpt (Oper.Float64FromBEBytes, e) } %prec unopt_prec 
  | FLOAT32_FROM_LE_BYTES; e = expr_target;
    { Expr.UnOpt (Oper.Float32FromLEBytes, e) } %prec unopt_prec 
  | FLOAT32_FROM_BE_BYTES  e = expr_target;
    { Expr.UnOpt (Oper.Float32FromBEBytes, e) } %prec unopt_prec
  | INT_FROM_BYTES; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.IntFromBytes, e1, e2) }
  | UINT_FROM_BYTES; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.UintFromBytes, e1, e2) }
  | BYTES_TO_STRING  e = expr_target;
    { Expr.UnOpt (Oper.BytesToString, e) } %prec unopt_prec
  | FLOAT_TO_BYTE  e = expr_target;
    { Expr.UnOpt (Oper.FloatToByte, e) } %prec unopt_prec
  | ARRAY_MAKE; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.ArrayMake, e1, e2) }
  | ANTH; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Oper.Anth, e1, e2) }
  | ASET; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; COMMA; e3 = expr_target; RPAREN;
    { Expr.TriOpt (Oper.Aset, e1, e2, e3) }
  | LSET; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; COMMA; e3 = expr_target; RPAREN;
    { Expr.TriOpt (Oper.Lset, e1, e2, e3) }
  | ALEN; e = expr_target;
    { Expr.UnOpt (Oper.ArrayLen, e) } %prec unopt_prec
  | LIST_TO_ARRAY; e = expr_target;
    { Expr.UnOpt (Oper.ListToArray, e) } %prec unopt_prec

stmt_block:
  | s = separated_list (SEMICOLON, stmt_target);
    { Stmt.Block s }

(* s ::= e.f := e | delete e.f | skip | x := e | s1; s2 | if (e) { s1 } else { s2 } | while (e) { s } | return e | return *)
stmt_target:
  | PRINT; e = expr_target;
    { Stmt.Print e }
  | FAIL; e = expr_target;
    { Stmt.Fail e }
  | ABORT; e = expr_target;
    { Stmt.Abort e }
  | ASSUME; LPAREN; e = expr_target; RPAREN;
    { Stmt.Assume e }
  | ASSERT; LPAREN; e = expr_target; RPAREN;
    { Stmt.Assert e }
  | THROW; str = STRING;
    { Stmt.Exception str}
  | e1 = expr_target; PERIOD; f = VAR; DEFEQ; e2 = expr_target;
    { Stmt.FieldAssign (e1, Expr.Val (Val.Str f), e2) }
  | e1 = expr_target; LBRACK; f = expr_target; RBRACK; DEFEQ; e2 = expr_target;
    { Stmt.FieldAssign (e1, f, e2) }
  | DELETE; e = expr_target; PERIOD; f = VAR;
    { Stmt.FieldDelete (e, Expr.Val (Val.Str f)) }
  | DELETE; e = expr_target; LBRACK; f = expr_target; RBRACK;
    { Stmt.FieldDelete (e, f) }
  | SKIP;
    { Stmt.Skip }
  | v = VAR; DEFEQ; e = expr_target;
    { Stmt.Assign (v, e) }
  | exps_stmts = ifelse_target;
    { exps_stmts }
  | WHILE; LPAREN; e = expr_target; RPAREN; LBRACE; s = stmt_block; RBRACE;
    { Stmt.While (e, s) }
  | RETURN; e = expr_target;
    { Stmt.Return e }
  | RETURN;
    { Stmt.Return (Expr.Val Val.Void) }
  | v = VAR; DEFEQ; f = expr_target; LPAREN; vs = separated_list(COMMA, expr_target); RPAREN;
    { Stmt.AssignCall (v,f,vs)}
  | x = VAR; DEFEQ; EXTERN; f = VAR; LPAREN; vs = separated_list(COMMA, expr_target); RPAREN;
    { Stmt.AssignECall (x,f,vs)}
  | v = VAR; DEFEQ; e1 = expr_target; IN_OBJ; e2 = expr_target;
    { Stmt.AssignInObjCheck (v,e1,e2)}
  | v = VAR; DEFEQ; e = expr_target; PERIOD; f = VAR;
    { Stmt.FieldLookup (v,e, Expr.Val (Val.Str f)) }
  | v = VAR; DEFEQ; e = expr_target; LBRACK; f = expr_target; RBRACK;
    { Stmt.FieldLookup (v,e, f) }
  | v = VAR; DEFEQ; LBRACE; RBRACE;
    { Stmt.AssignNewObj (v) }
  | v = VAR; DEFEQ; OBJ_TO_LIST; e = expr_target;
    { Stmt.AssignObjToList (v, e) }
  | v = VAR; DEFEQ; OBJ_FIELDS; e = expr_target;
    { Stmt.AssignObjFields (v, e) }



(* if (e) { s } | if (e) {s} else { s } *)
ifelse_target:
  | IF; LPAREN; e = expr_target; RPAREN; LBRACE; s1 = stmt_block; RBRACE; ELSE;LBRACE; s2 = stmt_block; RBRACE;
    { Stmt.If(e, s1, Some s2) }
  | IF; LPAREN; e = expr_target; RPAREN; LBRACE; s = stmt_block; RBRACE;
    { Stmt.If(e, s, None) }

%inline op_target:
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
