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
    
  let fresh_lambda_id_gen = String_utils.make_fresh_var_generator "__lambda__"

%}

(* ========== Typed tokens ========== *)

%token <int>    INT
%token <float>  FLOAT
%token <string> STRING
%token <bool>   BOOLEAN
%token <string> SYMBOL
%token <string> LOC
%token <string> ID
%token <string> GVAR

(* ========== Language tokens ========== *)

%token NULL NONE
%token IMPORT MACRO
%token PRINT DELETE
%token FUNCTION RETURN EXTERN LAMBDA
%token IF ELSE ELIF
%token WHILE FOREACH REPEAT UNTIL
%token SWITCH CASE SDEFAULT
%token MATCH WITH DEFAULT 
%token THROW CATCH
%token FAIL ASSERT
%token WRAPPER

(* ========== Symbol tokens ========== *)

%token PERIOD COMMA SEMICOLON COLON
%token DEFEQ
%token ATSIGN HASH
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token LARRBRACK RARRBRACK
%token PLUS MINUS TIMES DIVIDE MODULO POW
%token TILDE AMPERSAND PIPE CARET
%token SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%token QUESTION EXCLAMATION
%token LAND LOR SCLAND SCLOR
%token EQ LT GT LE GE
%token RIGHT_ARROW
%token EOF

(* ========== Operator tokens ========== *)

%token MAX_VALUE MIN_VALUE PI

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

(* ========== Type system tokens ========== *)

%token TYPEDEF
%token STYPE_ANY, STYPE_UNKNOWN, STYPE_NEVER
%token STYPE_UNDEFINED, STYPE_VOID
%token STYPE_INT, STYPE_FLOAT, STYPE_STRING, STYPE_BOOLEAN STYPE_SYMBOL
%token STYPE_SIGMA

(* ========== Precedence and Associativity ========== *)

%left LAND LOR SCLAND SCLOR 
%left EQ
%left LT GT LE GE 
%left AMPERSAND PIPE CARET SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%left OBJECT_MEM LIST_MEM
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right POW

%nonassoc unopt_prec
%nonassoc PERIOD LBRACK

(* ========== Entry Point ========== *)

%type <EExpr.t> entry_expr_target
%type <EStmt.t> entry_stmt_target
%type <EFunc.t> entry_func_target
%type <EProg.t> entry_prog_target

%start entry_prog_target entry_func_target entry_stmt_target entry_expr_target 






(* ======================================= *)
(*            Grammar and rules            *)
(* ======================================= *)

%% 

entry_expr_target:
  | e = expr_target; EOF;   { e };

entry_stmt_target:
  | s = stmt_target; EOF;   { s };

entry_func_target:
  | f = func_target; EOF;   { f };

entry_prog_target:
  | p = prog_target; EOF;   { p };

(* ==================== Program  ==================== *)

prog_target:
  | imports = list(import_target); prog_els = separated_list(SEMICOLON, e_prog_elem_target);
   {
    let split_els = fun el (type_decls', funcs', macros') ->
      match el with
        | EProg.ElementType.TypeDeclaration el' -> (el'::type_decls', funcs', macros')
        | EProg.ElementType.Procedure el' -> (type_decls', el'::funcs', macros')
        | EProg.ElementType.Macro el' -> (type_decls', funcs', el'::macros')
    in
    let (type_decls, funcs, macros) = List.fold_right split_els prog_els ([], [], []) in
    EProg.create imports type_decls funcs macros
   }
  ;

import_target:
  | IMPORT; fname = STRING; SEMICOLON;
    { fname }
  ;

e_prog_elem_target:
  | t = typedef_target;
    { EProg.ElementType.TypeDeclaration t }
  | f = func_target;
    { EProg.ElementType.Procedure f }
  | m = macro_target;
    { EProg.ElementType.Macro m }

(* ==================== Type definitions ==================== *)

typedef_target:
  | TYPEDEF; v = ID; DEFEQ; t = e_type_target;
    { (v, t) }

(* ==================== Functions ==================== *)

func_target:
  | FUNCTION; fn = ID; LPAREN; vars = params_target; RPAREN;
    tret = option(e_typing_target); s = block_target;
    { EFunc.create None fn vars tret s @> at $sloc }
  | FUNCTION; fn = ID; LPAREN; vars = params_target; RPAREN; meta = delimited(LBRACK, vals_metadata_target, RBRACK); 
    vars_meta = vars_opt_metadata_target; tret = option(e_typing_target); s = block_target;
    { EFunc.create (Some (E_Func_Metadata.build_func_metadata meta vars_meta)) fn vars tret s @> at $sloc }

params_target:
  | params = separated_list(COMMA, param_target);
    { params }
  ;

param_target:
  | v = ID; t = option(e_typing_target)
    { (v, t) }

(* ==================== Macros ==================== *)

macro_target:
  | MACRO; m = ID; LPAREN; vars = separated_list(COMMA, ID); RPAREN; s = block_target;
   { EMacro.create m vars s @> at $sloc }
  ;

(* ==================== Statements ==================== *)

block_target:
  | LBRACE; stmts = separated_list (SEMICOLON, stmt_target); RBRACE;
    { EStmt.Block stmts @> at $sloc }
  ;

stmt_target:
  | e = expr_target;
    { EStmt.ExprStmt e @> at $sloc }
  | HASH; s= stmt_target;
    { EStmt.Debug s @> at $sloc }
  | PRINT; e = expr_target;
    { EStmt.Print e @> at $sloc }
  | RETURN; e = option(expr_target);
    { EStmt.Return e @> at $sloc }
  | x = ID; t = option(e_typing_target); DEFEQ; e = expr_target;
    { EStmt.Assign (x, t, e) @> at $sloc }
  | x = GVAR; DEFEQ; e = expr_target;
    { EStmt.GlobAssign (x, e) @> at $sloc }
  | oe = expr_target; fe = lookup_target; DEFEQ; e = expr_target;
    { EStmt.FieldAssign (oe, fe, e) @> at $sloc }
  | DELETE; oe = expr_target; fe = lookup_target;
    { EStmt.FieldDelete (oe, fe) @> at $sloc }
  | if_case = if_target; else_case = option(else_target);
    {
      let (e, s1, meta1) = if_case in
      let (s2, meta2) = match else_case with | None -> (None, []) | Some (s, meta) -> (Some s, meta) in
      EStmt.If (e, s1, s2, meta1, meta2) @> at $sloc
    }
  | if_case = if_target; elif_cases = nonempty_list(elif_target); else_case = option(else_target);
    { EStmt.EIf (if_case :: elif_cases, else_case) @> at $sloc }
  | WHILE; LPAREN; e = expr_target; RPAREN; s = block_target;
    { EStmt.While (e, s) @> at $sloc }
  | FOREACH; LPAREN; x = ID; COLON; e = expr_target; RPAREN; s = block_target;
    { EStmt.ForEach (x, e, s, [], None) @> at $sloc }
  | FOREACH; LPAREN; x = ID; COLON; e = expr_target; RPAREN; 
    stmt_meta = delimited(LBRACK, stmt_metadata_target, RBRACK);
    var_meta = var_opt_metadata_target; s = block_target;
    { EStmt.ForEach (x, e, s, stmt_meta, var_meta) @> at $sloc }
  | REPEAT; meta = stmt_opt_metadata_target; s = block_target; e = option(until_target);
    { EStmt.RepeatUntil (s, (Option.value ~default:(EExpr.Val (Val.Bool false)) e), meta) @> at $sloc }
  | SWITCH; LPAREN; e = expr_target; RPAREN; meta = switch_case_opt_metadata_target; LBRACE; 
    cases = list(switch_case_target); default_case = option(switch_case_default_target) RBRACE;
    { EStmt.Switch (e, cases, default_case, meta) @> at $sloc }
  | MATCH; e = expr_target; WITH; PIPE; match_cases = separated_list(PIPE, match_case_target);
    { EStmt.MatchWith (e, match_cases) @> at $sloc }
  | x = ID; option(e_typing_target); DEFEQ; LAMBDA; LPAREN; xs = separated_list(COMMA, ID); RPAREN;
    LBRACK; ys = separated_list(COMMA, ID); RBRACK; s = block_target;
    { EStmt.Lambda (x, fresh_lambda_id_gen (), xs, ys, s) @> at $sloc }
  | ATSIGN; m = ID; LPAREN; es = separated_list(COMMA, expr_target); RPAREN;
    { EStmt.MacroApply (m, es) @> at $sloc }
  | THROW; e = expr_target;
    { EStmt.Throw e @> at $sloc }
  | FAIL; e = expr_target;
    { EStmt.Fail e @> at $sloc }
  | ASSERT; e = expr_target;
    { EStmt.Assert e @> at $sloc }
  | WRAPPER; meta = stmt_opt_metadata_target; s = block_target;
    { EStmt.Wrapper (meta, s) @> at $sloc }
  ;

if_target:
  | IF; LPAREN; e = expr_target; RPAREN; meta = stmt_opt_metadata_target; s = block_target;
    { (e, s, meta) }
  ;

elif_target:
  | ELIF; LPAREN; e = expr_target; RPAREN; meta = stmt_opt_metadata_target; s = block_target;
    { (e, s, meta) }
  ;

else_target:
  | ELSE; meta = stmt_opt_metadata_target; s = block_target;
    { (s, meta) }
  ;

until_target:
  | UNTIL; e = expr_target;
    { e }
  ;

switch_case_target:
  | CASE; e = expr_target; COLON; s = block_target;
    { (e, s) }

switch_case_default_target:
  | SDEFAULT; COLON; s = stmt_target;
    { s }

(* ==================== Patterns ==================== *)

match_case_target:
  | p = pattern_target; RIGHT_ARROW; s = block_target;
    { (p, s) }

pattern_target:
  | LBRACE; pbs = separated_list(COMMA, pattern_binding_target); RBRACE;
    { EPat.ObjPat (pbs, None) @> at $sloc }
  | LBRACE; pbs = separated_list(COMMA, pattern_binding_target); RBRACE; 
    meta = delimited(LBRACK, vals_metadata_target, RBRACK); vars_meta = vars_opt_metadata_target;
    { EPat.ObjPat (pbs, (Some (EPat_metadata.build_pat_metadata meta vars_meta))) @> at $sloc }
  | DEFAULT;
    { EPat.DefaultPat @> at $sloc }

pattern_binding_target:
  | pn = ID; COLON; pv = pattern_value_target;        { (pn, pv) }
  | pn = STRING; COLON; pv = pattern_value_target;    { (pn, pv) }

pattern_value_target:
  | v = ID;               { EPat.PatVar v }
  | v = val_target;       { EPat.PatVal v }
  | LBRACK; RBRACK;       { EPat.PatVal (Val.List []) }
  | NONE;                 { EPat.PatNone }

(* ==================== Expressions ==================== *)

expr_target:
  | LPAREN; e = expr_target; RPAREN;
    { e }
  | v = val_target;
    { EExpr.Val v }
  | v = ID;
    { EExpr.Var v }
  | v = GVAR;
    { EExpr.GVar v }
  | const = const_target;
    { EExpr.Const const }
  | unopt = unopt_infix_target; e = expr_target;   %prec unopt_prec
    { EExpr.UnOpt (unopt, e) }
  | unopt = unopt_call_target; e = expr_target;    %prec unopt_prec
    { EExpr.UnOpt (unopt, e) }
  | e1 = expr_target; binopt = binopt_infix_target; e2 = expr_target;
    { EExpr.BinOpt (binopt, e1, e2) }
  | binopt = binopt_call_target; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { EExpr.BinOpt (binopt, e1, e2) }
  | triopt = triopt_call_target; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; COMMA; e3 = expr_target; RPAREN;
    { EExpr.TriOpt (triopt, e1, e2, e3) }
  | nopt_expr = nopt_target;
    { nopt_expr }
  | fn = ID; LPAREN; es = separated_list(COMMA, expr_target); RPAREN; handler = option(catch_target);
    { EExpr.Call (EExpr.Val (Val.Str fn), es, handler) }
  | LBRACE; fe = expr_target; RBRACE; LPAREN; es = separated_list(COMMA, expr_target); RPAREN; handler = option(catch_target);
    { EExpr.Call (fe, es, handler) }
  | EXTERN; fn = ID; LPAREN; es = separated_list(COMMA, expr_target); RPAREN;
    { EExpr.ECall (fn, es) }
  | LBRACE; flds = separated_list(COMMA, fld_init_target); RBRACE;
    { EExpr.NewObj (flds) }
  | oe = expr_target; fe = lookup_target
    { EExpr.Lookup (oe, fe) }
  | LBRACE; fe = expr_target; RBRACE; ATSIGN; LPAREN; es = separated_list(COMMA, expr_target); RPAREN;
    { EExpr.Curry (fe, es) }
  ;

nopt_target:
  | LARRBRACK; es = separated_list (COMMA, expr_target); RARRBRACK;
    { EExpr.NOpt (ArrayExpr, es) }
  | LBRACK; es = separated_list (COMMA, expr_target); RBRACK;
    { EExpr.NOpt (ListExpr, es) }
  | LPAREN; t = tuple_target; RPAREN;
    { EExpr.NOpt (TupleExpr, List.rev t) }
  ;

catch_target:
  | CATCH; handler = ID
    { handler }
  ;

fld_init_target:
  | fn = ID; COLON; fe = expr_target;
    { (fn, fe) }
  | fn = STRING; COLON; fe = expr_target;
    { (fn, fe) }
  ;

lookup_target:
  | PERIOD; fn = ID;
    { EExpr.Val (Val.Str fn) }
  | LBRACK; fe = expr_target; RBRACK;
    { fe }

(* ==================== Values ==================== *)

val_target:
  | NULL;               { Val.Null }
  | i = INT;            { Val.Int i }
  | f = FLOAT;          { Val.Flt f }
  | s = STRING;         { Val.Str s }
  | b = BOOLEAN;        { Val.Bool b }
  | s = SYMBOL;         { Val.Symbol s }
  | l = LOC;            { Val.Loc l }
  | t = dtype_target;   { Val.Type t }
  ;

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
  ;

tuple_target:
  | v1 = expr_target; COMMA; v2 = expr_target;
    { [v2; v1] }
  | vs = tuple_target; COMMA; v = expr_target;
    { v :: vs }
  ;

(* ==================== Operators ==================== *)

%inline const_target:
  | MAX_VALUE;              { Operator.MAX_VALUE }
  | MIN_VALUE;              { Operator.MIN_VALUE }
  | PI;                     { Operator.PI }

%inline unopt_infix_target:
  | MINUS                   { Operator.Neg }
  | EXCLAMATION             { Operator.LogicalNot }
  | TILDE                   { Operator.BitwiseNot }
  ;

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
  | SCLAND                  { Operator.SCLogicalAnd }
  | SCLOR                   { Operator.SCLogicalOr }
  | EQ                      { Operator.Eq }
  | LT                      { Operator.Lt }
  | GT                      { Operator.Gt }
  | LE                      { Operator.Le }
  | GE                      { Operator.Ge }
  | OBJECT_MEM              { Operator.ObjectMem }
  | LIST_MEM                { Operator.ListMem }
  ;

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
  | OBJECT_TO_LIST          { Operator.ObjectToList }
  | OBJECT_FIELDS           { Operator.ObjectFields }
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
  ;

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
  ;

%inline triopt_call_target:
  | STRING_SUBSTR           { Operator.StringSubstr }
  | STRING_SUBSTR_U         { Operator.StringSubstrU }
  | ARRAY_SET               { Operator.ArraySet }
  | LIST_SET                { Operator.ListSet }
  ;

(* ==================== Metadata ==================== *)

vals_metadata_target:
  | meta = separated_list(COMMA, val_target);
    { meta }
  ;

var_metadata_target:
  | meta = STRING;
    {
      let param_alt = String.split_on_char ':' meta in
      if List.length param_alt = 2 then ( List.nth param_alt 0, List.nth param_alt 1 )
      else raise (Failure "Invalid function's variables metadata")
    }
  ;

stmt_metadata_target:
  | meta = separated_list(COMMA, STRING);
    { List.map (
        fun (m : string) : EStmt.metadata_t ->
          let sep_idx = String.index_opt m ':' in
          match sep_idx with
          | None   -> { where = m; html = "" }
          | Some idx ->
            let where = String.sub m 0 idx in
            let html = String.sub m (idx+1) ((String.length m)-idx-1) in
            { where; html }
      ) meta
    }
  ;

var_opt_metadata_target:
  | meta = option(delimited(LBRACK, var_metadata_target, RBRACK))
    { meta }
  ;

vars_opt_metadata_target:
  | meta = option(delimited(LBRACK, separated_list(COMMA, var_metadata_target), RBRACK));
    { Option.value ~default:[] meta }
  ;

stmt_opt_metadata_target:
  | meta = option(delimited(LBRACK, stmt_metadata_target, RBRACK))
    { Option.value ~default:[] meta }
  ;

switch_case_opt_metadata_target:
  | meta = option(delimited(LBRACK, STRING, RBRACK))
    { Option.value ~default:"" meta }
  ;

(* ==================== Type system ==================== *)

e_typing_target:
  | COLON; t = e_type_target;
    { t }

e_type_target:
  | t = e_simple_type_target;
    { t }
  | t = e_nary_type_target;
    { t }

e_simple_type_target:
  | LPAREN; t = e_type_target; RPAREN;
    { t }
  | STYPE_ANY;
    { EType.AnyType }
  | STYPE_UNKNOWN;
    { EType.UnknownType }
  | STYPE_NEVER;
    { EType.NeverType }
  | STYPE_UNDEFINED;
    { EType.UndefinedType }
  | STYPE_VOID;
    { EType.VoidType }
  | STYPE_INT;
    { EType.IntType }
  | STYPE_FLOAT;
    { EType.FloatType }
  | STYPE_STRING;
    { EType.StringType }
  | STYPE_BOOLEAN;
    { EType.BooleanType }
  | STYPE_SYMBOL;
    { EType.SymbolType }
  | v = val_target;
    { EType.parse_literal_type v }
  | LBRACK; RBRACK;
    { EType.parse_literal_type (Val.List []) }
  | LBRACK; t = e_type_target; RBRACK;
    { EType.ListType t }
  | LBRACE; props = separated_list (COMMA, e_type_property_target); RBRACE;
    { EType.ObjectType (EType.parse_obj_type props) }
  | v = ID;
    { EType.UserDefinedType v }

e_nary_type_target:
  | t1 = e_simple_type_target; merge_func = e_nary_type_op_target; t2 = e_type_target;
    { merge_func t1 t2 }
  | STYPE_SIGMA; LBRACK; d = ID; RBRACK; option(PIPE); t = e_nary_type_target;
    { EType.parse_sigma_type d t }

e_nary_type_op_target:
  | TIMES;        { EType.merge_tuple_type }
  | PIPE;         { EType.merge_union_type }

e_type_property_target:
  | v = ID; COLON; t = e_type_target;
    { EType.Field.NamedField (v, (t, false) ) }
  | v = ID; QUESTION; COLON; t = e_type_target;
    { EType.Field.NamedField (v, (t, true) ) }
  | TIMES; COLON; t = e_type_target;
    { EType.Field.SumryField t }
