(* ================================= *)
(*            Definitions            *)
(* ================================= *)

%{
  open Source

  let position_to_pos position =
    {
      line   = position.Lexing.pos_lnum;
      column = position.Lexing.pos_cnum - position.Lexing.pos_bol;
    }

  let at (startpos, endpos) =
    { 
      file  = startpos.Lexing.pos_fname; 
      left  = position_to_pos startpos; 
      right = position_to_pos endpos;
    }

%}

(* ========== Typed tokens ========== *)

%token <int>    INT
%token <float>  FLOAT
%token <string> STRING
%token <bool>   BOOLEAN
%token <string> SYMBOL
%token <string> LOC
%token <string> ID

(* ========== Language tokens ========== *)

%token NULL
%token PRINT DELETE
%token FUNCTION RETURN EXTERN
%token IF ELSE 
%token WHILE
%token FAIL ASSERT

(* ========== Symbol tokens ========== *)

%token PERIOD COMMA SEMICOLON
%token DEFEQ
%token ATSIGN HASH
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token LARRBRACK RARRBRACK
%token PLUS MINUS TIMES DIVIDE MODULO POW
%token TILDE AMPERSAND PIPE CARET
%token SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%token EXCLAMATION 
%token LAND LOR
%token EQ LT GT LE GE
%token EOF

(* ========== Operator tokens ========== *)

%token TYPEOF

%token INT_TO_FLOAT INT_TO_STRING INT_TO_FOUR_HEX OCTAL_TO_DECIMAL
%token FLOAT_TO_INT FLOAT_TO_STRING TO_INT TO_INT32 TO_UINT16 TO_UINT32 IS_NAN
%token TO_PRECISION TO_EXPONENTIAL TO_FIXED

%token STRING_TO_INT STRING_TO_FLOAT FROM_CHAR_CODE FROM_CHAR_CODE_U TO_CHAR_CODE TO_CHAR_CODE_U TO_LOWER_CASE TO_UPPER_CASE TRIM
%token STRING_LEN STRING_LEN_U STRING_CONCAT
%token STRING_NTH STRING_NTH_U STRING_SPLIT
%token STRING_SUBSTR STRING_SUBSTR_U

%token OBJECT_TO_LIST OBJECT_FIELDS 
%token OBJECT_MEM

%token ARRAY_LEN
%token ARRAY_MAKE ARRAY_NTH
%token ARRAY_SET

%token LIST_TO_ARRAY LIST_HEAD LIST_TAIL LIST_LEN LIST_SORT LIST_REVERSE LIST_REMOVE_LAST
%token LIST_MEM LIST_NTH LIST_ADD LIST_PREPEND LIST_CONCAT LIST_REMOVE LIST_REMOVE_NTH
%token LIST_SET

%token TUPLE_FIRST TUPLE_SECOND TUPLE_LEN
%token TUPLE_NTH

%token FLOAT_TO_BYTE
%token FLOAT32_TO_LE_BYTES FLOAT32_TO_BE_BYTES FLOAT64_TO_LE_BYTES FLOAT64_TO_BE_BYTES
%token FLOAT32_FROM_LE_BYTES FLOAT32_FROM_BE_BYTES FLOAT64_FROM_LE_BYTES FLOAT64_FROM_BE_BYTES
%token BYTES_TO_STRING
%token INT_TO_BE_BYTES INT_FROM_LE_BYTES UINT_FROM_LE_BYTES

%token RANDOM ABS SQRT CEIL FLOOR EXP LOG_2 LOG_E LOG_10
%token SIN COS TAN SINH COSH TANH ASIN ACOS ATAN
%token MAX MIN ATAN_2

%token UTF8_DECODE HEX_DECODE
%token PARSE_NUMBER PARSE_STRING PARSE_DATE

(* ========== Runtime type tokens ========== *)

%token DTYPE_NULL
%token DTYPE_INT DTYPE_FLT DTYPE_STR DTYPE_BOOL DTYPE_SYMBOL 
%token DTYPE_LOC DTYPE_LIST DTYPE_TUPLE DTYPE_CURRY

(* ========== Precedence and associativity ========== *)

%left LAND LOR
%left EQ
%left LT GT LE GE 
%left AMPERSAND PIPE CARET SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%left LIST_MEM
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right POW

%nonassoc unopt_prec

(* ========== Entry point ========== *)

%type <Expr.t> entry_expr_target
%type <Stmt.t> entry_stmt_target
%type <Func.t> entry_func_target
%type <Prog.t> entry_prog_target

%start entry_prog_target entry_func_target entry_stmt_target entry_expr_target  






(* ======================================= *)
(*            Grammar and rules            *)
(* ======================================= *)

%%

entry_expr_target:
  | e = expr_target; EOF;   { e }

entry_stmt_target:
  | s = stmt_target; EOF;   { s }

entry_func_target:
  | f = func_target; EOF;   { f }

entry_prog_target:
  | p = prog_target; EOF;   { p }

(* ==================== Program  ==================== *)

prog_target:
  | fs = separated_list(SEMICOLON, func_target);
    { Prog.create fs }

(* ==================== Functions ==================== *)

func_target:
  | FUNCTION; fn = id_target; LPAREN; pxs = separated_list(COMMA, id_target); RPAREN; s = block_target;
    { Func.create fn pxs s @> at $sloc }

(* ==================== Statements ==================== *)

block_target:
  | LBRACE; ss = separated_list (SEMICOLON, stmt_target); RBRACE;
    { Stmt.Block ss @> at $sloc }

stmt_target:
  | HASH; s = stmt_target;
    { Stmt.Debug (s) @> at $sloc }
  | PRINT; e = expr_target;
    { Stmt.Print e @> at $sloc }
  | RETURN;
    { Stmt.Return (Expr.Val Val.Void @> no_region) @> at $sloc }
  | RETURN; e = expr_target;
    { Stmt.Return e @> at $sloc }
  | x = id_target; DEFEQ; e = expr_target;
    { Stmt.Assign (x, e) @> at $sloc }
  | x = id_target; DEFEQ; fn = expr_target; LPAREN; vs = separated_list(COMMA, expr_target); RPAREN;
    { Stmt.AssignCall (x, fn, vs) @> at $sloc }
  | x = id_target; DEFEQ; EXTERN; fn = id_target; LPAREN; vs = separated_list(COMMA, expr_target); RPAREN;
    { Stmt.AssignECall (x, fn, vs) @> at $sloc }
  | x = id_target; DEFEQ; LBRACE; RBRACE;
    { Stmt.AssignNewObj x @> at $sloc }
  | x = id_target; DEFEQ; OBJECT_TO_LIST; e = expr_target;
    { Stmt.AssignObjToList (x, e) @> at $sloc }
  | x = id_target; DEFEQ; OBJECT_FIELDS; e = expr_target;
    { Stmt.AssignObjFields (x, e) @> at $sloc }
  | x = id_target; DEFEQ; e1 = expr_target; OBJECT_MEM; e2 = expr_target;
    { Stmt.AssignInObjCheck (x, e1, e2) @> at $sloc }
  | x = id_target; DEFEQ; oe = expr_target; fe = lookup_target;
    { Stmt.FieldLookup (x, oe, fe) @> at $sloc }
  | oe = expr_target; fe = lookup_target; DEFEQ; e = expr_target;
    { Stmt.FieldAssign (oe, fe, e) @> at $sloc }
  | DELETE; oe = expr_target; fe = lookup_target;
    { Stmt.FieldDelete (oe, fe) @> at $sloc }
  | IF; LPAREN; e = expr_target; RPAREN; s = block_target;
    { Stmt.If (e, s, None) @> at $sloc }
  | IF; LPAREN; e = expr_target; RPAREN; s1 = block_target; ELSE; s2 = block_target;
    { Stmt.If (e, s1, Some s2) @> at $sloc }
  | WHILE; LPAREN; e = expr_target; RPAREN; s = block_target;
    { Stmt.While (e, s) @> at $sloc }
  | FAIL; e = expr_target;
    { Stmt.Fail e @> at $sloc }
  | ASSERT; e = expr_target;
    { Stmt.Assert e @> at $sloc }

lookup_target:
  | PERIOD; fn = id_target;             { Expr.Val (Val.Str fn.it) @> at $sloc }
  | LBRACK; fe = expr_target; RBRACK;   { fe }

(* ==================== Expressions ==================== *)

expr_target:
  | LPAREN; e = expr_target; RPAREN;
    { e }
  | v = val_target;
    { Expr.Val v @> at $sloc }
  | x = ID;
    { Expr.Var x @> at $sloc }
  | unopt = unopt_infix_target; e = expr_target;   %prec unopt_prec
    { Expr.UnOpt (unopt, e) @> at $sloc }
  | unopt = unopt_call_target; e = expr_target;    %prec unopt_prec
    { Expr.UnOpt (unopt, e) @> at $sloc }
  | e1 = expr_target; binopt = binopt_infix_target; e2 = expr_target;
    { Expr.BinOpt (binopt, e1, e2) @> at $sloc }
  | binopt = binopt_call_target; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (binopt, e1, e2) @> at $sloc }
  | triopt = triopt_call_target; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; COMMA; e3 = expr_target; RPAREN;
    { Expr.TriOpt (triopt, e1, e2, e3) @> at $sloc }
  | nopt_expr = nopt_target;
    { nopt_expr }
  | LBRACE; fe = expr_target; RBRACE; ATSIGN; LPAREN; es = separated_list(COMMA, expr_target); RPAREN;
    { Expr.Curry (fe, es) @> at $sloc }
  ;

nopt_target:
  | LARRBRACK; es = separated_list (COMMA, expr_target); RARRBRACK;
    { Expr.NOpt (ArrayExpr, es) @> at $sloc }
  | LBRACK; es = separated_list (COMMA, expr_target); RBRACK;
    { Expr.NOpt (ListExpr, es) @> at $sloc }
  | LPAREN; vs = tuple_target; RPAREN;
    { Expr.NOpt (TupleExpr, List.rev vs) @> at $sloc }
  ;

(* ==================== Values ==================== *)

id_target:
  | x = ID;             { (x @> at $sloc) }

val_target:
  | NULL;               { Val.Null }
  | i = INT;            { Val.Int i }
  | f = FLOAT;          { Val.Flt f }
  | s = STRING;         { Val.Str s }
  | b = BOOLEAN;        { Val.Bool b }
  | s = SYMBOL;         { Val.Symbol s }
  | l = LOC;            { Val.Loc l }
  | t = dtype_target;   { Val.Type t }

dtype_target:
  | DTYPE_NULL;         { Type.NullType }
  | DTYPE_INT;          { Type.IntType }
  | DTYPE_FLT;          { Type.FltType }
  | DTYPE_STR;          { Type.StrType }
  | DTYPE_BOOL;         { Type.BoolType }
  | DTYPE_SYMBOL;       { Type.SymbolType }
  | DTYPE_LOC;          { Type.LocType }
  | DTYPE_LIST;         { Type.ListType }
  | DTYPE_TUPLE;        { Type.TupleType }
  | DTYPE_CURRY;        { Type.CurryType }

tuple_target:
  | v1 = expr_target; COMMA; v2 = expr_target;
    { [v2; v1] }
  | vs = tuple_target; COMMA; v = expr_target;
    { v :: vs }
  ;

(* ==================== Operators ==================== *)

%inline unopt_infix_target:
  | MINUS                   { Operator.Neg }
  | EXCLAMATION             { Operator.LogicalNot }
  | TILDE                   { Operator.BitwiseNot }

%inline binopt_infix_target:
  | PLUS                    { Operator.Plus }
  | MINUS                   { Operator.Minus }
  | TIMES                   { Operator.Times }
  | DIVIDE                  { Operator.Div }
  | MODULO                  { Operator.Modulo }
  | POW                     { Operator.Pow }
  | AMPERSAND               { Operator.BitwiseAnd }
  | PIPE                    { Operator.BitwiseOr }
  | CARET                   { Operator.BitwiseXor }
  | SHIFT_LEFT              { Operator.ShiftLeft }
  | SHIFT_RIGHT             { Operator.ShiftRight }
  | SHIFT_RIGHT_LOGICAL     { Operator.ShiftRightLogical }
  | LAND                    { Operator.LogicalAnd }
  | LOR                     { Operator.LogicalOr }
  | EQ                      { Operator.Eq }
  | LT                      { Operator.Lt }
  | GT                      { Operator.Gt }
  | LE                      { Operator.Le }
  | GE                      { Operator.Ge }
  | LIST_MEM                { Operator.ListMem }

%inline unopt_call_target:
  | TYPEOF                  { Operator.Typeof }
  | INT_TO_FLOAT            { Operator.IntToFloat }
  | INT_TO_STRING           { Operator.IntToString }
  | INT_TO_FOUR_HEX         { Operator.IntToFourHex }
  | OCTAL_TO_DECIMAL        { Operator.OctalToDecimal }
  | FLOAT_TO_INT            { Operator.FloatToInt }
  | FLOAT_TO_STRING         { Operator.FloatToString }
  | TO_INT                  { Operator.ToInt }
  | TO_INT32                { Operator.ToInt32 }
  | TO_UINT16               { Operator.ToUint16 }
  | TO_UINT32               { Operator.ToUint32 }
  | IS_NAN                  { Operator.IsNaN }
  | STRING_TO_INT           { Operator.StringToInt }
  | STRING_TO_FLOAT         { Operator.StringToFloat }
  | FROM_CHAR_CODE          { Operator.FromCharCode }
  | FROM_CHAR_CODE_U        { Operator.FromCharCodeU }
  | TO_CHAR_CODE            { Operator.ToCharCode }
  | TO_CHAR_CODE_U          { Operator.ToCharCodeU }
  | TO_LOWER_CASE           { Operator.ToLowerCase }
  | TO_UPPER_CASE           { Operator.ToUpperCase }
  | TRIM                    { Operator.Trim }
  | STRING_LEN              { Operator.StringLen }
  | STRING_LEN_U            { Operator.StringLenU }
  | STRING_CONCAT           { Operator.StringConcat }
  | ARRAY_LEN               { Operator.ArrayLen }
  | LIST_TO_ARRAY           { Operator.ListToArray }
  | LIST_HEAD               { Operator.ListHead }
  | LIST_TAIL               { Operator.ListTail }
  | LIST_LEN                { Operator.ListLen }
  | LIST_SORT               { Operator.ListSort }
  | LIST_REVERSE            { Operator.ListReverse }
  | LIST_REMOVE_LAST        { Operator.ListRemoveLast }
  | TUPLE_FIRST             { Operator.TupleFirst }
  | TUPLE_SECOND            { Operator.TupleSecond }
  | TUPLE_LEN               { Operator.TupleLen }
  | FLOAT_TO_BYTE           { Operator.FloatToByte }
  | FLOAT32_TO_LE_BYTES     { Operator.Float32ToLEBytes }
  | FLOAT32_TO_BE_BYTES     { Operator.Float32ToBEBytes }
  | FLOAT64_TO_LE_BYTES     { Operator.Float64ToLEBytes }
  | FLOAT64_TO_BE_BYTES     { Operator.Float64ToBEBytes }
  | FLOAT32_FROM_LE_BYTES   { Operator.Float32FromLEBytes }
  | FLOAT32_FROM_BE_BYTES   { Operator.Float32FromBEBytes }
  | FLOAT64_FROM_LE_BYTES   { Operator.Float64FromLEBytes }
  | FLOAT64_FROM_BE_BYTES   { Operator.Float64FromBEBytes }
  | BYTES_TO_STRING         { Operator.BytesToString }
  | RANDOM                  { Operator.Random }
  | ABS                     { Operator.Abs }
  | SQRT                    { Operator.Sqrt }
  | CEIL                    { Operator.Ceil }
  | FLOOR                   { Operator.Floor }
  | EXP                     { Operator.Exp }
  | LOG_2                   { Operator.Log2 }
  | LOG_E                   { Operator.LogE }
  | LOG_10                  { Operator.Log10 }
  | SIN                     { Operator.Sin }
  | COS                     { Operator.Cos }
  | TAN                     { Operator.Tan }
  | SINH                    { Operator.Sinh }
  | COSH                    { Operator.Cosh }
  | TANH                    { Operator.Tanh }
  | ASIN                    { Operator.Asin }
  | ACOS                    { Operator.Acos }
  | ATAN                    { Operator.Atan }
  | UTF8_DECODE             { Operator.Utf8Decode }
  | HEX_DECODE              { Operator.HexDecode }
  | PARSE_NUMBER            { Operator.ParseNumber }
  | PARSE_STRING            { Operator.ParseString }
  | PARSE_DATE              { Operator.ParseDate }

%inline binopt_call_target:
  | TO_PRECISION            { Operator.ToPrecision }
  | TO_EXPONENTIAL          { Operator.ToExponential }
  | TO_FIXED                { Operator.ToFixed }
  | STRING_NTH              { Operator.StringNth }
  | STRING_NTH_U            { Operator.StringNthU }
  | STRING_SPLIT            { Operator.StringSplit }
  | ARRAY_MAKE              { Operator.ArrayMake }
  | ARRAY_NTH               { Operator.ArrayNth }
  | LIST_NTH                { Operator.ListNth }
  | LIST_ADD                { Operator.ListAdd }
  | LIST_PREPEND            { Operator.ListPrepend }
  | LIST_CONCAT             { Operator.ListConcat }
  | LIST_REMOVE             { Operator.ListRemove }
  | LIST_REMOVE_NTH         { Operator.ListRemoveNth }
  | TUPLE_NTH               { Operator.TupleNth }
  | INT_TO_BE_BYTES         { Operator.IntToBEBytes }
  | INT_FROM_LE_BYTES       { Operator.IntFromLEBytes }
  | UINT_FROM_LE_BYTES      { Operator.UintFromLEBytes }
  | MIN                     { Operator.Min }
  | MAX                     { Operator.Max }
  | ATAN_2                  { Operator.Atan2 }

%inline triopt_call_target:
  | STRING_SUBSTR           { Operator.StringSubstr }
  | STRING_SUBSTR_U         { Operator.StringSubstrU }
  | ARRAY_SET               { Operator.ArraySet }
  | LIST_SET                { Operator.ListSet }
