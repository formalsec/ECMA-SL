(* parser-specification file *)
%{
open Source
open Operators

let position_to_pos position =
  {
    file = position.Lexing.pos_fname;
    line = position.Lexing.pos_lnum;
    column = position.Lexing.pos_cnum - position.Lexing.pos_bol;
  }

let position_to_region pos1 pos2 =
  { left = position_to_pos pos1; right = position_to_pos pos2 }

let at (startpos, endpos) =
  position_to_region startpos endpos

%}
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
%token ASSERT
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

%token API_ASSUME API_MK_SYMBOLIC API_ABORT
%token API_EVALUATE API_MAXIMIZE API_MINIMIZE
%token API_IS_SYMBOLIC API_IS_SAT API_IS_NUMBER

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
  ;

proc_target:
  | FUNCTION; f = VAR; LPAREN; vars = separated_list (COMMA, VAR); RPAREN; LBRACE; s = stmt_block; RBRACE
   { Func.create f vars s }
  ;

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
  ;

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
  ;

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
  ;

(* e ::= {} | {f:e} | [] | [e] | e.f | e[f] | v | x | -e | e+e | f(e) | (e) *)
expr_target:
  | API_MK_SYMBOLIC; LPAREN; t = type_target; COMMA; x = expr_target; RPAREN;
    { Expr.Symbolic (t, x) }
  | LBRACK; es = separated_list (COMMA, expr_target); RBRACK;
    { Expr.NOpt (ListExpr, es) }
  | LARRBRACK; es = separated_list (COMMA, expr_target); RARRBRACK;
    { Expr.NOpt (ArrExpr, es) }
  | LPAREN; t = tuple_target; RPAREN;
    { Expr.NOpt (TupleExpr, List.rev t) }
  | v = val_target;
    { Expr.Val v }
  | v = VAR;
    { Expr.Var v }
  | LBRACE; e = expr_target; RBRACE; AT_SIGN; LPAREN; es = separated_list (COMMA, expr_target); RPAREN;
    { Expr.Curry (e, es) }
  | MINUS; e = expr_target;
    { Expr.UnOpt (Neg, e) } %prec unopt_prec
  | NOT; e = expr_target;
    { Expr.UnOpt (Not, e) } %prec unopt_prec
  | IS_NAN; e = expr_target;
    { Expr.UnOpt (IsNaN, e) } %prec unopt_prec
  | BITWISE_NOT; e = expr_target;
    { Expr.UnOpt (BitwiseNot, e) } %prec unopt_prec
  | LLEN; e = expr_target;
    { Expr.UnOpt (ListLen, e) } %prec unopt_prec
  | TLEN; e = expr_target;
    { Expr.UnOpt (TupleLen, e) } %prec unopt_prec
  | SLEN; e = expr_target;
    { Expr.UnOpt (StringLen, e) } %prec unopt_prec
  | SLEN_U; e = expr_target;
    { Expr.UnOpt (StringLenU, e) } %prec unopt_prec
  | TYPEOF; e = expr_target;
    { Expr.UnOpt (Typeof, e) } %prec unopt_prec
  | HD; e = expr_target;
    { Expr.UnOpt (Head, e) } %prec unopt_prec
  | TL; e = expr_target;
    { Expr.UnOpt (Tail, e) } %prec unopt_prec
  | FST; e = expr_target;
    { Expr.UnOpt (First, e) } %prec unopt_prec
  | SND; e = expr_target;
    { Expr.UnOpt (Second, e) } %prec unopt_prec
  | LREMOVELAST; e = expr_target;
    { Expr.UnOpt (LRemoveLast, e) } %prec unopt_prec
  | LSORT; e = expr_target;
    { Expr.UnOpt (LSort, e) } %prec unopt_prec
  | INT_TO_FLOAT; e = expr_target;
    { Expr.UnOpt (IntToFloat, e) } %prec unopt_prec
  | INT_TO_STRING; e = expr_target;
    { Expr.UnOpt (IntToString, e) } %prec unopt_prec
  | INT_TO_FOUR_HEX; e = expr_target;
    { Expr.UnOpt (IntToFourHex, e) } %prec unopt_prec
  | HEX_DECODE; e = expr_target;
    { Expr.UnOpt (HexDecode, e) } %prec unopt_prec
  | UTF8_DECODE; e = expr_target;
    { Expr.UnOpt (Utf8Decode, e) } %prec unopt_prec
  | OCTAL_TO_DECIMAL; e = expr_target;
    { Expr.UnOpt (OctalToDecimal, e) } %prec unopt_prec
  | INT_OF_STRING; e = expr_target;
    { Expr.UnOpt (IntOfString, e) } %prec unopt_prec
  | INT_OF_FLOAT; e = expr_target;
    { Expr.UnOpt (IntOfFloat, e) } %prec unopt_prec
  | TO_INT; e = expr_target;
    { Expr.UnOpt (ToInt, e) } %prec unopt_prec
  | TO_INT32; e = expr_target;
    { Expr.UnOpt (ToInt32, e) } %prec unopt_prec
  | TO_UINT32; e = expr_target;
    { Expr.UnOpt (ToUint32, e) } %prec unopt_prec
  | FROM_CHAR_CODE; e = expr_target;
    { Expr.UnOpt (FromCharCode, e) } %prec unopt_prec
  | FROM_CHAR_CODE_U; e = expr_target;
    { Expr.UnOpt (FromCharCodeU, e) } %prec unopt_prec
  | TO_CHAR_CODE; e = expr_target;
    { Expr.UnOpt (ToCharCode, e) } %prec unopt_prec
  | TO_CHAR_CODE_U; e = expr_target;
    { Expr.UnOpt (ToCharCodeU, e) } %prec unopt_prec
  | TO_LOWER_CASE; e = expr_target;
    { Expr.UnOpt (ToLowerCase, e) } %prec unopt_prec
  | TO_UPPER_CASE; e = expr_target;
    { Expr.UnOpt (ToUpperCase, e) } %prec unopt_prec
  | TRIM; e = expr_target;
    { Expr.UnOpt (Trim, e) } %prec unopt_prec
  | TO_UINT16; e = expr_target;
    { Expr.UnOpt (ToUint16, e) } %prec unopt_prec
  | ABS; e = expr_target;
    { Expr.UnOpt (Abs, e) } %prec unopt_prec
  | ACOS; e = expr_target;
    { Expr.UnOpt (Acos, e) } %prec unopt_prec
  | ASIN; e = expr_target;
    { Expr.UnOpt (Asin, e) } %prec unopt_prec
  | ATAN; e = expr_target;
    { Expr.UnOpt (Atan, e) } %prec unopt_prec
  | CEIL; e = expr_target;
    { Expr.UnOpt (Ceil, e) } %prec unopt_prec
  | COS; e = expr_target;
    { Expr.UnOpt (Cos, e) } %prec unopt_prec
  | EXP; e = expr_target;
    { Expr.UnOpt (Exp, e) } %prec unopt_prec
  | FLOOR; e = expr_target;
    { Expr.UnOpt (Floor, e) } %prec unopt_prec
  | LOG_E; e = expr_target;
    { Expr.UnOpt (Log_e, e) } %prec unopt_prec
  | LOG_10; e = expr_target;
    { Expr.UnOpt (Log_10, e) } %prec unopt_prec
  | RANDOM; e = expr_target;
    { Expr.UnOpt (Random, e) } %prec unopt_prec
  | SIN; e = expr_target;
    { Expr.UnOpt (Sin, e) } %prec unopt_prec
  | SQRT; e = expr_target;
    { Expr.UnOpt (Sqrt, e) } %prec unopt_prec
  | TAN; e = expr_target;
    { Expr.UnOpt (Tan, e) } %prec unopt_prec
  | FLOAT_TO_STRING; e = expr_target;
    { Expr.UnOpt (FloatToString, e) } %prec unopt_prec
  | FLOAT_OF_STRING; e = expr_target;
    { Expr.UnOpt (FloatOfString, e) } %prec unopt_prec
  | e1 = expr_target; bop = op_target; e2 = expr_target;
    { Expr.BinOpt (bop, e1, e2) }
  | LNTH; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Lnth, e1, e2) }
  | LREM; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (LRem, e1, e2) }
  | LREMNTH; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (LRemNth, e1, e2) }
  | TNTH; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Tnth, e1, e2) }
  | SNTH; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Snth, e1, e2) }
  | SNTH_U; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Snth_u, e1, e2) }
  | SSPLIT; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Ssplit, e1, e2) }
  | SSUBSTR; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; COMMA; e3 = expr_target; RPAREN;
    { Expr.TriOpt (Ssubstr, e1, e2, e3) }
  | SSUBSTR_U; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; COMMA; e3 = expr_target; RPAREN;
    { Expr.TriOpt (SsubstrU, e1, e2, e3) }
  | LADD; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Ladd, e1, e2) }
  | LPREPEND; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Lprepend, e1, e2) }
  | LCONCAT; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Lconcat, e1, e2) }
  | LREVERSE; e = expr_target;
    { Expr.UnOpt (LReverse, e) } %prec unopt_prec
  | LPAREN; e = expr_target; RPAREN;
    { e }
  | SCONCAT; e = expr_target;
    { Expr.UnOpt (Sconcat, e) } %prec unopt_prec
  | ATAN_2; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Atan2, e1, e2) }
  | MAX; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Max, e1, e2) }
  | MIN; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Min, e1, e2) }
  | PARSE_NUMBER; e = expr_target;
    { Expr.UnOpt (ParseNumber, e) } %prec unopt_prec
  | PARSE_STRING; e = expr_target;
    { Expr.UnOpt (ParseString, e) } %prec unopt_prec
  | PARSE_DATE; e = expr_target;
    { Expr.UnOpt (ParseDate, e) } %prec unopt_prec
  | TO_PRECISION; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN; 
    { Expr.BinOpt (ToPrecision, e1, e2) }
  | TO_EXPONENTIAL; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN; 
    { Expr.BinOpt (ToExponential, e1, e2) }
  | TO_FIXED; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN; 
    { Expr.BinOpt (ToFixed, e1, e2) }
  | COSH;  e = expr_target;
    { Expr.UnOpt (Cosh, e) } %prec unopt_prec 
  | LOG_2;  e = expr_target;
    { Expr.UnOpt (Log_2, e) } %prec unopt_prec 
  | SINH;  e = expr_target;
    { Expr.UnOpt (Sinh, e) } %prec unopt_prec 
  | TANH;  e = expr_target;
    { Expr.UnOpt (Tanh, e) } %prec unopt_prec
  | FLOAT64_TO_LE_BYTES; e = expr_target;
    { Expr.UnOpt (Float64ToLEBytes, e) } %prec unopt_prec 
  | FLOAT64_TO_BE_BYTES; e = expr_target;
    { Expr.UnOpt (Float64ToBEBytes, e) } %prec unopt_prec 
  | FLOAT32_TO_LE_BYTES; e = expr_target;
    { Expr.UnOpt (Float32ToLEBytes, e) } %prec unopt_prec 
  | FLOAT32_TO_BE_BYTES; e = expr_target;
    { Expr.UnOpt (Float32ToBEBytes, e) } %prec unopt_prec 
  | INT_TO_BE_BYTES; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (IntToBEBytes, e1, e2) }
  | FLOAT64_FROM_LE_BYTES; e = expr_target;
    { Expr.UnOpt (Float64FromLEBytes, e) } %prec unopt_prec 
  | FLOAT64_FROM_BE_BYTES; e = expr_target;
    { Expr.UnOpt (Float64FromBEBytes, e) } %prec unopt_prec 
  | FLOAT32_FROM_LE_BYTES; e = expr_target;
    { Expr.UnOpt (Float32FromLEBytes, e) } %prec unopt_prec 
  | FLOAT32_FROM_BE_BYTES  e = expr_target;
    { Expr.UnOpt (Float32FromBEBytes, e) } %prec unopt_prec
  | INT_FROM_BYTES; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (IntFromBytes, e1, e2) }
  | UINT_FROM_BYTES; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (UintFromBytes, e1, e2) }
  | BYTES_TO_STRING  e = expr_target;
    { Expr.UnOpt (BytesToString, e) } %prec unopt_prec
  | FLOAT_TO_BYTE  e = expr_target;
    { Expr.UnOpt (FloatToByte, e) } %prec unopt_prec
  | ARRAY_MAKE; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (ArrayMake, e1, e2) }
  | ANTH; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (Anth, e1, e2) }
  | ASET; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; COMMA; e3 = expr_target; RPAREN;
    { Expr.TriOpt (Aset, e1, e2, e3) }
  | LSET; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; COMMA; e3 = expr_target; RPAREN;
    { Expr.TriOpt (Lset, e1, e2, e3) }
  | ALEN; e = expr_target;
    { Expr.UnOpt (ArrayLen, e) } %prec unopt_prec
  | LIST_TO_ARRAY; e = expr_target;
    { Expr.UnOpt (ListToArray, e) } %prec unopt_prec
  ;

stmt_block:
  | s = separated_list (SEMICOLON, stmt_target); 
    { Stmt.Block s @@ at $sloc }
  ;

(* s ::= e.f := e | delete e.f | skip | x := e | s1; s2 | if (e) { s1 } else { s2 } | while (e) { s } | return e | return *)
stmt_target:
  | PRINT; e = expr_target;
    { Stmt.Print e @@ at $sloc }
  | FAIL; e = expr_target;
    { Stmt.Fail e @@ at $sloc }
  | API_ABORT; e = expr_target;
    { Stmt.Abort e @@ at $sloc }
  | ASSERT; LPAREN; e = expr_target; RPAREN;
    { Stmt.Assert e @@ at $sloc }
  | THROW; str = STRING;
    { Stmt.Exception str @@ at $sloc }
  | e1 = expr_target; PERIOD; f = VAR; DEFEQ; e2 = expr_target;
    { Stmt.FieldAssign (e1, Expr.Val (Val.Str f), e2) @@ at $sloc }
  | e1 = expr_target; LBRACK; f = expr_target; RBRACK; DEFEQ; e2 = expr_target;
    { Stmt.FieldAssign (e1, f, e2) @@ at $sloc }
  | DELETE; e = expr_target; PERIOD; f = VAR;
    { Stmt.FieldDelete (e, Expr.Val (Val.Str f)) @@ at $sloc }
  | DELETE; e = expr_target; LBRACK; f = expr_target; RBRACK;
    { Stmt.FieldDelete (e, f) @@ at $sloc }
  | SKIP;
    { Stmt.Skip @@ at $sloc }
  | v = VAR; DEFEQ; e = expr_target;
    { Stmt.Assign (v, e) @@ at $sloc }
  | exps_stmts = ifelse_target;
    { exps_stmts }
  | WHILE; LPAREN; e = expr_target; RPAREN; LBRACE; s = stmt_block; RBRACE;
    { Stmt.While (e, s) @@ at $sloc }
  | RETURN; e = expr_target;
    { Stmt.Return e @@ at $sloc }
  | RETURN;
    { Stmt.Return (Expr.Val Val.Void) @@ at $sloc }
  | api_stmt = api_stmt_target; { api_stmt @@ at $sloc }
  | v = VAR; DEFEQ; f = expr_target; LPAREN; vs = separated_list(COMMA, expr_target); RPAREN;
    { Stmt.AssignCall (v,f,vs) @@ at $sloc }
  | x = VAR; DEFEQ; EXTERN; f = VAR; LPAREN; vs = separated_list(COMMA, expr_target); RPAREN;
    { Stmt.AssignECall (x,f,vs) @@ at $sloc }
  | v = VAR; DEFEQ; e1 = expr_target; IN_OBJ; e2 = expr_target;
    { Stmt.AssignInObjCheck (v,e1,e2) @@ at $sloc }
  | v = VAR; DEFEQ; e = expr_target; PERIOD; f = VAR;
    { Stmt.FieldLookup (v,e, Expr.Val (Val.Str f)) @@ at $sloc }
  | v = VAR; DEFEQ; e = expr_target; LBRACK; f = expr_target; RBRACK;
    { Stmt.FieldLookup (v,e, f) @@ at $sloc }
  | v = VAR; DEFEQ; LBRACE; RBRACE;
    { Stmt.AssignNewObj v @@ at $sloc }
  | v = VAR; DEFEQ; OBJ_TO_LIST; e = expr_target;
    { Stmt.AssignObjToList (v, e) @@ at $sloc }
  | v = VAR; DEFEQ; OBJ_FIELDS; e = expr_target;
    { Stmt.AssignObjFields (v, e) @@ at $sloc }
  ;

api_stmt_target:
  | API_ASSUME; LPAREN; e = expr_target; RPAREN;
    { Stmt.SymStmt (SymStmt.Assume e) }
  | v = VAR; DEFEQ; API_IS_SYMBOLIC; LPAREN; e = expr_target; RPAREN;
    { Stmt.SymStmt (SymStmt.Is_symbolic (v, e)) }
  | v = VAR; DEFEQ; API_IS_SAT; LPAREN; e = expr_target; RPAREN;
    { Stmt.SymStmt (SymStmt.Is_sat (v, e)) }
  | v = VAR; DEFEQ; API_IS_NUMBER; LPAREN; e = expr_target; RPAREN;
    { Stmt.SymStmt (SymStmt.Is_number (v, e)) }
  | v = VAR; DEFEQ; API_MAXIMIZE; LPAREN; e = expr_target; RPAREN;
    { Stmt.SymStmt (SymStmt.Maximize (v, e)) }
  | v = VAR; DEFEQ; API_MINIMIZE; LPAREN; e = expr_target; RPAREN;
    { Stmt.SymStmt (SymStmt.Minimize (v, e)) }
  | v = VAR; DEFEQ; API_EVALUATE; LPAREN; e = expr_target; RPAREN;
    { Stmt.SymStmt (SymStmt.Evaluate (v, e)) }
  ;

(* if (e) { s } | if (e) {s} else { s } *)
ifelse_target:
  | IF; LPAREN; e = expr_target; RPAREN; LBRACE; s1 = stmt_block; RBRACE; ELSE;LBRACE; s2 = stmt_block; RBRACE;
    { Stmt.If (e, s1, Some s2) @@ at $sloc }
  | IF; LPAREN; e = expr_target; RPAREN; LBRACE; s = stmt_block; RBRACE;
    { Stmt.If (e, s, None) @@ at $sloc }
  ;

%inline op_target:
  | MINUS   { Minus }
  | PLUS    { Plus }
  | TIMES   { Times }
  | DIVIDE  { Div }
  | MODULO  { Modulo }
  | EQUAL   { Eq }
  | GT      { Gt }
  | LT      { Lt }
  | EGT     { Ge }
  | ELT     { Le }
  | LAND    { Log_And }
  | BITWISE_AND { BitwiseAnd }
  | BITWISE_OR { BitwiseOr }
  | BITWISE_XOR { BitwiseXor }
  | SHIFT_LEFT { ShiftLeft }
  | SHIFT_RIGHT { ShiftRight }
  | SHIFT_RIGHT_LOGICAL { ShiftRightLogical }
  | LOR     { Log_Or }
  | IN_LIST { InList }
  | POW     { Pow }
  ;
