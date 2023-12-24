(* ================================= *)
(*            Definitions            *)
(* ================================= *)

%{
open Source
open Operator

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

(* ========== Types ========== *)

%token <int> INT
%token <float> FLOAT
%token <bool> BOOLEAN
%token <string> STRING
%token <string> SYMBOL
%token <string> LOC
%token <string> VAR

(* ========== Language Tokens ========== *)

%token EOF
%token PERIOD COMMA SEMICOLON
%token DEFEQ
%token ATSIGN
%token LARRBRACK RARRBRACK
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token PLUS MINUS TIMES DIVIDE MODULO POW
%token BITWISE_NOT BITWISE_AND BITWISE_OR BITWISE_XOR 
%token SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%token NOT LAND LOR
%token EQ LT GT LE GE
%token IN_OBJ IN_LIST

%token NULL
%token PRINT DELETE 
%token FUNCTION RETURN EXTERN
%token IF ELSE WHILE
%token FAIL ASSERT

(* ========== Operator Tokens ========== *)

%token TYPEOF

%token INT_TO_FLOAT INT_TO_STRING INT_TO_FOUR_HEX OCTAL_TO_DECIMAL
%token FLOAT_TO_INT FLOAT_TO_STRING TO_INT TO_INT32 TO_UINT16 TO_UINT32 IS_NAN
%token TO_PRECISION TO_EXPONENTIAL TO_FIXED

%token STRING_TO_INT STRING_TO_FLOAT FROM_CHAR_CODE FROM_CHAR_CODE_U TO_CHAR_CODE TO_CHAR_CODE_U TO_LOWER_CASE TO_UPPER_CASE TRIM
%token STRING_LEN STRING_LEN_U STRING_CONCAT
%token STRING_NTH STRING_NTH_U STRING_SPLIT 
%token STRING_SUBSTR STRING_SUBSTR_U

%token OBJECT_TO_LIST OBJECT_FIELDS

%token ARRAY_LEN 
%token ARRAY_MAKE ARRAY_NTH
%token ARRAY_SET

%token LIST_TO_ARRAY LIST_HEAD LIST_TAIL LIST_LEN LIST_SORT LIST_REVERSE LIST_REMOVE_LAST 
%token LIST_NTH LIST_ADD LIST_PREPEND LIST_CONCAT LIST_REMOVE LIST_REMOVE_NTH
%token LIST_SET

%token TUPLE_FST TUPLE_SND TUPLE_LEN
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

(* ========== Symbolic Execution Tokens ========== *)

%token SE_ABORT

%token NULL_TYPE INT_TYPE FLT_TYPE STR_TYPE BOOL_TYPE SYMBOL_TYPE 
%token LOC_TYPE LIST_TYPE TUPLE_TYPE CURRY_TYPE



(* ========== Precedence and Associativity ========== *)

%left LAND LOR
%left EQ
%left LT GT LE GE BITWISE_AND BITWISE_OR BITWISE_XOR SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL IN_LIST
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right POW

%nonassoc unopt_prec

(* ========== Entry Point ========== *)

%type <Func.t > proc_target
%type <Func.t list> prog_target
%start prog_target proc_target






(* ======================================= *)
(*            Grammar and Rules            *)
(* ======================================= *)

%%

prog_target:
  | funcs = separated_list (SEMICOLON, proc_target); EOF;
    { funcs }
  ;

proc_target:
  | FUNCTION; fn = VAR; LPAREN; vars = separated_list (COMMA, VAR); RPAREN; LBRACE; s = stmt_block_target; RBRACE
    { Func.create fn vars s }
  ;

(* ========== Statements ========== *)

stmt_block_target:
  | s = separated_list (SEMICOLON, stmt_target);
    { Stmt.Block s @> at $sloc }
  ;

stmt_target:
  | PRINT; e = expr_target;
    { Stmt.Print e @> at $sloc }
  | RETURN;
    { Stmt.Return (Expr.Val Val.Void) @> at $sloc }
  | RETURN; e = expr_target;
    { Stmt.Return e @> at $sloc }
  | x = VAR; DEFEQ; e = expr_target;
    { Stmt.Assign (x, e) @> at $sloc }
  | x = VAR; DEFEQ; fn = expr_target; LPAREN; vs = separated_list(COMMA, expr_target); RPAREN;
    { Stmt.AssignCall (x, fn, vs) @> at $sloc }
  | x = VAR; DEFEQ; EXTERN; fn = VAR; LPAREN; vs = separated_list(COMMA, expr_target); RPAREN;
    { Stmt.AssignECall (x, fn, vs) @> at $sloc }
  | x = VAR; DEFEQ; LBRACE; RBRACE;
    { Stmt.AssignNewObj x @> at $sloc }
  | x = VAR; DEFEQ; OBJECT_TO_LIST; e = expr_target;
    { Stmt.AssignObjToList (x, e) @> at $sloc }
  | x = VAR; DEFEQ; OBJECT_FIELDS; e = expr_target;
    { Stmt.AssignObjFields (x, e) @> at $sloc }
  | x = VAR; DEFEQ; e1 = expr_target; IN_OBJ; e2 = expr_target;
    { Stmt.AssignInObjCheck (x, e1, e2) @> at $sloc }
  | x = VAR; DEFEQ; oe = expr_target; PERIOD; fn = VAR;
    { Stmt.FieldLookup (x, oe, Expr.Val (Val.Str fn)) @> at $sloc }
  | x = VAR; DEFEQ; oe = expr_target; LBRACK; fe = expr_target; RBRACK;
    { Stmt.FieldLookup (x, oe, fe) @> at $sloc }
  | oe = expr_target; PERIOD; fn = VAR; DEFEQ; e = expr_target;
    { Stmt.FieldAssign (oe, Expr.Val (Val.Str fn), e) @> at $sloc }
  | oe = expr_target; LBRACK; fe = expr_target; RBRACK; DEFEQ; e = expr_target;
    { Stmt.FieldAssign (oe, fe, e) @> at $sloc }
  | DELETE; oe = expr_target; PERIOD; fn = VAR;
    { Stmt.FieldDelete (oe, Expr.Val (Val.Str fn)) @> at $sloc }
  | DELETE; oe = expr_target; LBRACK; fe = expr_target; RBRACK;
    { Stmt.FieldDelete (oe, fe) @> at $sloc }
  | IF; LPAREN; e = expr_target; RPAREN; LBRACE; s = stmt_block_target; RBRACE;
    { Stmt.If (e, s, None) @> at $sloc }
  | IF; LPAREN; e = expr_target; RPAREN; LBRACE; s1 = stmt_block_target; RBRACE; ELSE; LBRACE; s2 = stmt_block_target; RBRACE;
    { Stmt.If (e, s1, Some s2) @> at $sloc }
  | WHILE; LPAREN; e = expr_target; RPAREN; LBRACE; s = stmt_block_target; RBRACE;
    { Stmt.While (e, s) @> at $sloc }
  | FAIL; e = expr_target;
    { Stmt.Fail e @> at $sloc }
  | ASSERT; LPAREN; e = expr_target; RPAREN;
    { Stmt.Assert e @> at $sloc }
  | SE_ABORT; e = expr_target;
    { Stmt.Abort e @> at $sloc }
  ;

(* ========== Expressions ========== *)

expr_target:
  | LPAREN; e = expr_target; RPAREN;
    { e }
  | v = val_target;
    { Expr.Val v }
  | v = VAR;
    { Expr.Var v }
  | unopt = unopt_infix_target; e = expr_target;   %prec unopt_prec
    { Expr.UnOpt (unopt, e) }
  | e1 = expr_target; binopt = binopt_infix_target; e2 = expr_target;
    { Expr.BinOpt (binopt, e1, e2) }
  | unopt = unopt_call_target; e = expr_target;    %prec unopt_prec
    { Expr.UnOpt (unopt, e) }
  | binopt = binopt_call_target; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (binopt, e1, e2) }
  | triopt = triopt_call_target; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; COMMA; e3 = expr_target; RPAREN;
    { Expr.TriOpt (triopt, e1, e2, e3) }
  | LARRBRACK; es = separated_list (COMMA, expr_target); RARRBRACK;
    { Expr.NOpt (ArrayExpr, es) }
  | LBRACK; es = separated_list (COMMA, expr_target); RBRACK;
    { Expr.NOpt (ListExpr, es) }
  | LPAREN; t = tuple_target; RPAREN;
    { Expr.NOpt (TupleExpr, List.rev t) }
  | LBRACE; e = expr_target; RBRACE; ATSIGN; LPAREN; es = separated_list (COMMA, expr_target); RPAREN;
    { Expr.Curry (e, es) }
  ;

(* ========== Values ========== *)

val_target:
  | NULL;
    { Val.Null }
  | i = INT;
    { Val.Int i }
  | f = FLOAT;
    { Val.Flt f }
  | s = STRING;
    { Val.Str s }
  | b = BOOLEAN;
    { Val.Bool b }
  | s = SYMBOL;
    { Val.Symbol s }
  | l = LOC;
    { Val.Loc l }
  | t = type_target;
    { Val.Type t }
  ;

type_target:
  | NULL_TYPE;
    { Type.NullType }
  | INT_TYPE;
    { Type.IntType }
  | FLT_TYPE;
    { Type.FltType }
  | STR_TYPE;
    { Type.StrType }
  | BOOL_TYPE;
    { Type.BoolType }
  | SYMBOL_TYPE;
    { Type.SymbolType }
  | LOC_TYPE;
    { Type.LocType }
  | LIST_TYPE;
    { Type.ListType }
  | TUPLE_TYPE;
    { Type.TupleType }
  | CURRY_TYPE;
    { Type.CurryType }
  ;

tuple_target:
  | v1 = expr_target; COMMA; v2 = expr_target;
    { [v2; v1] }
  | vs = tuple_target; COMMA; v = expr_target;
    { v :: vs }
  ;

(* ========== Operators ========== *)

%inline unopt_infix_target:
  | MINUS                   { Neg }
  | NOT                     { LogicalNot }
  | BITWISE_NOT             { BitwiseNot }
  ;

%inline binopt_infix_target:
  | PLUS                    { Plus }
  | MINUS                   { Minus }
  | TIMES                   { Times }
  | DIVIDE                  { Div }
  | MODULO                  { Modulo }
  | POW                     { Pow }
  | BITWISE_AND             { BitwiseAnd }
  | BITWISE_OR              { BitwiseOr }
  | BITWISE_XOR             { BitwiseXor }
  | SHIFT_LEFT              { ShiftLeft }
  | SHIFT_RIGHT             { ShiftRight }
  | SHIFT_RIGHT_LOGICAL     { ShiftRightLogical }
  | LAND                    { LogicalAnd }
  | LOR                     { LogicalOr }
  | EQ                      { Eq }
  | LT                      { Lt }
  | GT                      { Gt }
  | LE                      { Le }
  | GE                      { Ge }
  | IN_LIST                 { ListMem }
  ;

%inline unopt_call_target:
  | TYPEOF                  { Typeof }
  | INT_TO_FLOAT            { IntToFloat }
  | INT_TO_STRING           { IntToString }
  | INT_TO_FOUR_HEX         { IntToFourHex }
  | OCTAL_TO_DECIMAL        { OctalToDecimal }
  | FLOAT_TO_INT            { FloatToInt }
  | FLOAT_TO_STRING         { FloatToString }
  | TO_INT                  { ToInt }
  | TO_INT32                { ToInt32 }
  | TO_UINT16               { ToUint16 }
  | TO_UINT32               { ToUint32 }
  | IS_NAN                  { IsNaN }
  | STRING_TO_INT           { StringToInt }
  | STRING_TO_FLOAT         { StringToFloat }
  | FROM_CHAR_CODE          { FromCharCode }
  | FROM_CHAR_CODE_U        { FromCharCodeU }
  | TO_CHAR_CODE            { ToCharCode }
  | TO_CHAR_CODE_U          { ToCharCodeU }
  | TO_LOWER_CASE           { ToLowerCase }
  | TO_UPPER_CASE           { ToUpperCase }
  | TRIM                    { Trim }
  | STRING_LEN              { StringLen }
  | STRING_LEN_U            { StringLenU }
  | STRING_CONCAT           { StringConcat }
  | ARRAY_LEN               { ArrayLen }
  | LIST_TO_ARRAY           { ListToArray }
  | LIST_HEAD               { ListHead }
  | LIST_TAIL               { ListTail }
  | LIST_LEN                { ListLen }
  | LIST_SORT               { ListSort }
  | LIST_REVERSE            { ListReverse }
  | LIST_REMOVE_LAST        { ListRemoveLast }
  | TUPLE_FST               { TupleFirst }
  | TUPLE_SND               { TupleSecond }
  | TUPLE_LEN               { TupleLen }
  | FLOAT_TO_BYTE           { FloatToByte }
  | FLOAT32_TO_LE_BYTES     { Float32ToLEBytes }
  | FLOAT32_TO_BE_BYTES     { Float32ToBEBytes }
  | FLOAT64_TO_LE_BYTES     { Float64ToLEBytes }
  | FLOAT64_TO_BE_BYTES     { Float64ToBEBytes }
  | FLOAT32_FROM_LE_BYTES   { Float32FromLEBytes }
  | FLOAT32_FROM_BE_BYTES   { Float32FromBEBytes }
  | FLOAT64_FROM_LE_BYTES   { Float64FromLEBytes }
  | FLOAT64_FROM_BE_BYTES   { Float64FromBEBytes }
  | BYTES_TO_STRING         { BytesToString }
  | RANDOM                  { Random }
  | ABS                     { Abs }
  | SQRT                    { Sqrt }
  | CEIL                    { Ceil }
  | FLOOR                   { Floor }
  | EXP                     { Exp }
  | LOG_2                   { Log2 }
  | LOG_E                   { LogE }
  | LOG_10                  { Log10 }
  | SIN                     { Sin }
  | COS                     { Cos }
  | TAN                     { Tan }
  | SINH                    { Sinh }
  | COSH                    { Cosh }
  | TANH                    { Tanh }
  | ASIN                    { Asin }
  | ACOS                    { Acos }
  | ATAN                    { Atan }
  | UTF8_DECODE             { Utf8Decode }
  | HEX_DECODE              { HexDecode }
  | PARSE_NUMBER            { ParseNumber }
  | PARSE_STRING            { ParseString }
  | PARSE_DATE              { ParseDate }
  ;

%inline binopt_call_target:
  | TO_PRECISION            { ToPrecision }
  | TO_EXPONENTIAL          { ToExponential }
  | TO_FIXED                { ToFixed }
  | STRING_NTH              { StringNth }
  | STRING_NTH_U            { StringNthU }
  | STRING_SPLIT            { StringSplit }
  | ARRAY_MAKE              { ArrayMake }
  | ARRAY_NTH               { ArrayNth }
  | LIST_NTH                { ListNth }
  | LIST_ADD                { ListAdd }
  | LIST_PREPEND            { ListPrepend }
  | LIST_CONCAT             { ListConcat }
  | LIST_REMOVE             { ListRemove }
  | LIST_REMOVE_NTH         { ListRemoveNth }
  | TUPLE_NTH               { TupleNth }
  | INT_TO_BE_BYTES         { IntToBEBytes }
  | INT_FROM_LE_BYTES       { IntFromLEBytes }
  | UINT_FROM_LE_BYTES      { UintFromLEBytes }
  | MIN                     { Min }
  | MAX                     { Max }
  | ATAN_2                  { Atan2 }
  ;

%inline triopt_call_target:
  | STRING_SUBSTR           { StringSubstr }
  | STRING_SUBSTR_U         { StringSubstrU }
  | ARRAY_SET               { ArraySet }
  | LIST_SET                { ListSet }
  ;
