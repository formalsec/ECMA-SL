(* parser-specification file *)


%{
open E_Stmt
open Source
open Operators

let fresh_lambda_id_gen = String_utils.make_fresh_var_generator "__lambda__"

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
%token PRINT WRAPPER
<<<<<<< HEAD
%token ASSERT ASSUME SYMBOLIC IS_SYMBOLIC IS_NUMBER MAXIMIZE MINIMIZE ISSAT EVAL
=======
>>>>>>> toy-ecma
%token DEFEQ
%token WHILE FOREACH
%token IF ELSE ELIF
%token RETURN
%token SWITCH SDEFAULT
%token NULL
%token FUNCTION
%token MACRO
%token AT_SIGN
(*%token LARRBRACK RARRBRACK*)
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token PERIOD COMMA SEMICOLON COLON
%token DELETE
%token REPEAT UNTIL
%token MATCH WITH RIGHT_ARROW NONE DEFAULT CASE LAMBDA EXTERN
%token ASSERT
%token <float> FLOAT
%token <int> INT
%token <bool> BOOLEAN
%token <string> VAR
%token <string> GVAR
%token <string> STRING
%token <string> SYMBOL
%token <string> LOC
%token LAND LOR SCLAND SCLOR
%token PARSE_NUMBER PARSE_STRING PARSE_DATE INT_TO_FLOAT INT_TO_STRING INT_TO_FOUR_HEX HEX_DECODE UTF8_DECODE OCTAL_TO_DECIMAL
%token INT_OF_STRING FLOAT_OF_STRING FLOAT_TO_STRING OBJ_TO_LIST OBJ_FIELDS INT_OF_FLOAT
%token BITWISE_NOT BITWISE_AND PIPE BITWISE_XOR SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%token FROM_CHAR_CODE FROM_CHAR_CODE_U TO_CHAR_CODE TO_CHAR_CODE_U TO_LOWER_CASE TO_UPPER_CASE TRIM
%token TO_INT TO_INT32 TO_UINT32 TO_UINT16
%token ABS ACOS ASIN ATAN ATAN_2 CEIL COS EXP FLOOR LOG_E LOG_10 MAX MIN POW RANDOM SIN SQRT TAN PI MAX_VALUE MIN_VALUE COSH LOG_2 SINH TANH FLOAT64_TO_LE_BYTES FLOAT64_TO_BE_BYTES FLOAT32_TO_LE_BYTES FLOAT32_TO_BE_BYTES INT_TO_BE_BYTES FLOAT64_FROM_LE_BYTES FLOAT64_FROM_BE_BYTES FLOAT32_FROM_LE_BYTES FLOAT32_FROM_BE_BYTES INT_FROM_BYTES UINT_FROM_BYTES BYTES_TO_STRING FLOAT_TO_BYTE IS_NAN
%token PLUS MINUS TIMES DIVIDE MODULO EQUAL GT LT EGT ELT IN_OBJ IN_LIST TO_PRECISION TO_EXPONENTIAL TO_FIXED
%token NOT LLEN LNTH LREMNTH LREM LSET LADD LPREPEND LCONCAT LREVERSE HD TL TLEN TNTH FST SND LREMOVELAST LSORT SLEN SLEN_U SNTH SNTH_U SSUBSTR SSUBSTR_U ARRAY_MAKE ANTH ASET ALEN LIST_TO_ARRAY
%token SCONCAT SSPLIT
%token IMPORT THROW FAIL CATCH
%token TYPEOF INT_TYPE FLT_TYPE BOOL_TYPE STR_TYPE LOC_TYPE
%token LIST_TYPE TUPLE_TYPE NULL_TYPE SYMBOL_TYPE CURRY_TYPE
%token EOF


%token API_ASSUME API_MK_SYMBOLIC API_ABORT
%token API_EVAL API_MAXIMIZE API_MINIMIZE
%token API_IS_SYMBOLIC API_IS_SAT

%left SCLAND SCLOR LAND LOR
%left EQUAL
%left GT LT EGT ELT IN_OBJ IN_LIST BITWISE_AND PIPE BITWISE_XOR SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right POW

%nonassoc unopt_prec
%nonassoc PERIOD LBRACK

%type <E_Expr.t> e_prog_e_expr_target
%type <E_Stmt.t> e_prog_e_stmt_target
%type <E_Func.t> e_prog_e_func_target
%type <E_Prog.t> e_prog_target

%start e_prog_target e_prog_e_expr_target e_prog_e_stmt_target e_prog_e_func_target
%% (* separator line *)
(* END first section - declarations *)

(*
  BEGIN - second section - grammar and rules
  - specifying the grammar of the language to be parsed, specifying the productions
  - productions are organized into rules, where each rule lists all
    the possible productions for a given nonterminal.
*)

e_prog_e_expr_target:
  | e = e_expr_target; EOF; { e }
  ;

e_prog_e_stmt_target:
  | s = e_block_target; EOF; { s }
  ;

e_prog_e_func_target:
  | f = proc_target; EOF; { f }
  ;

e_prog_target:
  | imports = list (import_target); macros_funcs = separated_list (SEMICOLON, e_prog_elem_target); EOF;
   {
    let (funcs, macros) = List.split macros_funcs in
    let funcs' = List.concat (List.map (fun o -> Option.map_default (fun x -> [ x ]) [] o) funcs) in
    let macros' = List.concat (List.map (fun o -> Option.map_default (fun x -> [ x ]) [] o) macros) in
    E_Prog.create imports funcs' macros'
   }
  ;

import_target:
  | IMPORT; fname = STRING; SEMICOLON;
    { fname }
  ;

e_prog_elem_target:
  | f = proc_target;
    { (Some f, None) }
  | m = macro_target;
    { (None, Some m) }
  ;

proc_target:
  | FUNCTION; f = VAR; LPAREN; vars = proc_params_target; RPAREN; s = e_block_target;
   { E_Func.create None f vars s }
  | FUNCTION; f = VAR; LPAREN; vars = proc_params_target; RPAREN; meta = metadata_target; vars_meta_opt = option(vars_metadata_target); s = e_block_target;
   {
     let vars_meta = Option.default [] vars_meta_opt in
     let metadata = E_Func_Metadata.build_func_metadata meta vars_meta in
     E_Func.create (Some metadata) f vars s }
  ;

proc_params_target:
  | params = separated_list (COMMA, VAR);
    { params }
  (* | params = separated_list (COMMA, VAR); COMMA; LBRACK; fparams = separated_list(COMMA, VAR); RBRACK;
    { params @ fparams } *)
  ;

macro_target:
  | MACRO; m = VAR; LPAREN; vars = separated_list (COMMA, VAR); RPAREN; s = e_block_target;
   { E_Macro.create m vars s }
  ;

metadata_target:
  | LBRACK; meta = separated_list (COMMA, val_target); RBRACK;
    { meta }
  ;

vars_metadata_target:
  | LBRACK; meta = separated_list (COMMA, var_metadata_target); RBRACK;
    { meta }
  ;

var_metadata_target:
  | meta = STRING;
    {
      let param_alt = String.split_on_char ':' meta in
      match List.length param_alt with
      | 2 -> ( List.nth param_alt 0, List.nth param_alt 1 )
      | _ -> raise (Failure "Invalid function's variables metadata")
    }
  ;

(*
  The pipes separate the individual productions, and the curly braces contain a semantic action:
    OCaml code that generates the OCaml value corresponding to the production in question.
  Semantic actions are arbitrary OCaml expressions that are evaluated during parsing
    to produce values that are attached to the nonterminal in the rule.
*)

tuple_target:
  | v1 = e_expr_target; COMMA; v2 = e_expr_target;
    { [v2; v1] }
  | vs = tuple_target; COMMA; v = e_expr_target;
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
    (* This replaces helps on fixing errors when parsing some escape characters. *)
    { Val.Str s }
  | s = SYMBOL;
    { Val.Symbol s }
  | l = LOC;
    { Val.Loc l }
  | t = type_target;
    { Val.Type t }
  ;

(* e ::= {} | {f:e} | [] | [e] | e.f | e[f] | v | x | -e | e+e | f(e) | (e) *)
e_expr_target:
  | LBRACE; fes = separated_list (COMMA, fv_target); RBRACE;
    { E_Expr.NewObj (fes) }
  | e = e_expr_target; PERIOD; f = VAR;
    { E_Expr.Lookup (e, E_Expr.Val (Val.Str f)) }
  | e = e_expr_target; LBRACK; f = e_expr_target; RBRACK;
    { E_Expr.Lookup (e, f) }
  | v = val_target;
    { E_Expr.Val v }
  | v = VAR;
    { E_Expr.Var v }
  | v = GVAR;
    { E_Expr.GVar v }
  | MAX_VALUE;
    { E_Expr.Const MAX_VALUE }
  | MIN_VALUE;
    { E_Expr.Const MIN_VALUE }
  | PI;
    { E_Expr.Const PI }
  | EXTERN; f = VAR; LPAREN; es = separated_list (COMMA, e_expr_target); RPAREN;
    { E_Expr.ECall (f, es) }
  | f = VAR; LPAREN; es = separated_list (COMMA, e_expr_target); RPAREN; CATCH; g = VAR;
    { E_Expr.Call (E_Expr.Val (Val.Str f), es, Some g) }
  | LBRACE; f = e_expr_target; RBRACE; LPAREN; es = separated_list (COMMA, e_expr_target); RPAREN; CATCH; g=VAR;
    { E_Expr.Call (f, es, Some g) }
  | f = VAR; LPAREN; es = separated_list (COMMA, e_expr_target); RPAREN;
    { E_Expr.Call (E_Expr.Val (Val.Str f), es, None) }
  | LBRACE; f = e_expr_target; RBRACE; LPAREN; es = separated_list (COMMA, e_expr_target); RPAREN;
    { E_Expr.Call (f, es, None) }
  | LBRACE; f = e_expr_target; RBRACE; AT_SIGN; LPAREN; es = separated_list (COMMA, e_expr_target); RPAREN;
    { E_Expr.Curry (f, es) }
  | LPAREN; e = e_expr_target; RPAREN;
    { e }
  | nary_op_expr = nary_op_target;
    { nary_op_expr }
  | pre_un_op_expr = prefix_unary_op_target;
    { pre_un_op_expr }
  | pre_bin_op_expr = prefix_binary_op_target;
    { pre_bin_op_expr }
  | pre_tri_op_expr = prefix_trinary_op_target;
    { pre_tri_op_expr }
  | in_bin_op_expr = infix_binary_op_target;
    { in_bin_op_expr }
  | se_op_target; { $1 }
  ;

se_op_target:
  | API_MK_SYMBOLIC; LPAREN; t = type_target; COMMA; x = e_expr_target; RPAREN;
    { E_Expr.Symbolic (t, x) }
  | API_EVAL; LPAREN; e = e_expr_target; RPAREN; 
    { E_Expr.SymOpt (E_Expr.Evaluate e) }
  | API_MAXIMIZE; LPAREN; e = e_expr_target; RPAREN; 
    { E_Expr.SymOpt (E_Expr.Maximize e) }
  | API_MINIMIZE; LPAREN; e = e_expr_target; RPAREN; 
    { E_Expr.SymOpt (E_Expr.Minimize e) }
  | API_IS_SAT; LPAREN; e = e_expr_target; RPAREN;
    { E_Expr.SymOpt (E_Expr.Is_sat e) }
  | API_IS_SYMBOLIC; LPAREN; e = e_expr_target; RPAREN; 
    { E_Expr.SymOpt (E_Expr.Is_symbolic e) }
  ;

nary_op_target:
  | LBRACK; es = separated_list (COMMA, e_expr_target); RBRACK;
    { E_Expr.NOpt (ListExpr, es) }
  | LPAREN; t = tuple_target; RPAREN;
    { E_Expr.NOpt (TupleExpr, List.rev t) }
  (*| LARRBRACK; es = separated_list (COMMA, e_expr_target); RARRBRACK;
    { E_Expr.NOpt (ArrExpr, es) }*)
  ;

prefix_unary_op_target:
  | MINUS; e = e_expr_target;
    { E_Expr.UnOpt (Neg, e) } %prec unopt_prec
  | NOT; e = e_expr_target;
    { E_Expr.UnOpt (Not, e) } %prec unopt_prec
  | IS_NAN; e = e_expr_target;
    { E_Expr.UnOpt (IsNaN, e) } %prec unopt_prec
  | BITWISE_NOT; e = e_expr_target;
    { E_Expr.UnOpt (BitwiseNot, e) } %prec unopt_prec
  | LLEN; e = e_expr_target;
    { E_Expr.UnOpt (ListLen, e) } %prec unopt_prec
  | TLEN; e = e_expr_target;
    { E_Expr.UnOpt (TupleLen, e) } %prec unopt_prec
  | SLEN; e = e_expr_target;
    { E_Expr.UnOpt (StringLen, e) } %prec unopt_prec
  | SLEN_U; e = e_expr_target;
    { E_Expr.UnOpt (StringLenU, e) } %prec unopt_prec
  | TYPEOF; e = e_expr_target;
    { E_Expr.UnOpt (Typeof, e) } %prec unopt_prec
  | HD; e = e_expr_target;
    { E_Expr.UnOpt (Head, e) } %prec unopt_prec
  | TL; e = e_expr_target;
    { E_Expr.UnOpt (Tail, e) } %prec unopt_prec
  | FST; e = e_expr_target;
    { E_Expr.UnOpt (First, e) } %prec unopt_prec
  | SND; e = e_expr_target;
    { E_Expr.UnOpt (Second, e) } %prec unopt_prec
  | LREMOVELAST; e = e_expr_target;
    { E_Expr.UnOpt (LRemoveLast, e) } %prec unopt_prec
  | LSORT; e = e_expr_target;
    { E_Expr.UnOpt (LSort, e) } %prec unopt_prec
  | INT_TO_FLOAT; e = e_expr_target;
    { E_Expr.UnOpt (IntToFloat, e) } %prec unopt_prec
  | INT_TO_STRING; e = e_expr_target;
    { E_Expr.UnOpt (IntToString, e) } %prec unopt_prec
  | INT_TO_FOUR_HEX; e = e_expr_target;
    { E_Expr.UnOpt (IntToFourHex, e) } %prec unopt_prec
  | HEX_DECODE; e = e_expr_target;
    { E_Expr.UnOpt (HexDecode, e) } %prec unopt_prec
  | UTF8_DECODE; e = e_expr_target;
    { E_Expr.UnOpt (Utf8Decode, e) } %prec unopt_prec
  | OCTAL_TO_DECIMAL; e = e_expr_target;
    { E_Expr.UnOpt (OctalToDecimal, e) } %prec unopt_prec
  | INT_OF_STRING; e = e_expr_target;
    { E_Expr.UnOpt (IntOfString, e) } %prec unopt_prec
  | INT_OF_FLOAT; e = e_expr_target;
    { E_Expr.UnOpt (IntOfFloat, e) } %prec unopt_prec
  | FLOAT_TO_STRING; e = e_expr_target;
    { E_Expr.UnOpt (FloatToString, e) } %prec unopt_prec
  | FLOAT_OF_STRING; e = e_expr_target;
    { E_Expr.UnOpt (FloatOfString, e) } %prec unopt_prec
  | TO_INT; e = e_expr_target;
    { E_Expr.UnOpt (ToInt, e) } %prec unopt_prec
  | TO_INT32; e = e_expr_target;
    { E_Expr.UnOpt (ToInt32, e) } %prec unopt_prec
  | TO_UINT32; e = e_expr_target;
    { E_Expr.UnOpt (ToUint32, e) } %prec unopt_prec
  | FROM_CHAR_CODE; e = e_expr_target;
    { E_Expr.UnOpt (FromCharCode, e) } %prec unopt_prec
  | FROM_CHAR_CODE_U; e = e_expr_target;
    { E_Expr.UnOpt (FromCharCodeU, e) } %prec unopt_prec
  | TO_CHAR_CODE; e = e_expr_target;
    { E_Expr.UnOpt (ToCharCode, e) } %prec unopt_prec
  | TO_CHAR_CODE_U; e = e_expr_target;
    { E_Expr.UnOpt (ToCharCodeU, e) } %prec unopt_prec
  | TO_LOWER_CASE; e = e_expr_target;
    { E_Expr.UnOpt (ToLowerCase, e) } %prec unopt_prec
  | TO_UPPER_CASE; e = e_expr_target;
    { E_Expr.UnOpt (ToUpperCase, e) } %prec unopt_prec
  | TRIM; e = e_expr_target;
    { E_Expr.UnOpt (Trim, e) } %prec unopt_prec
  | TO_UINT16; e = e_expr_target;
    { E_Expr.UnOpt (ToUint16, e) } %prec unopt_prec
  | ABS; e = e_expr_target;
    { E_Expr.UnOpt (Abs, e) } %prec unopt_prec
  | ACOS; e = e_expr_target;
    { E_Expr.UnOpt (Acos, e) } %prec unopt_prec
  | ASIN; e = e_expr_target;
    { E_Expr.UnOpt (Asin, e) } %prec unopt_prec
  | ATAN; e = e_expr_target;
    { E_Expr.UnOpt (Atan, e) } %prec unopt_prec
  | CEIL; e = e_expr_target;
    { E_Expr.UnOpt (Ceil, e) } %prec unopt_prec
  | COS; e = e_expr_target;
    { E_Expr.UnOpt (Cos, e) } %prec unopt_prec
  | EXP; e = e_expr_target;
    { E_Expr.UnOpt (Exp, e) } %prec unopt_prec
  | FLOOR; e = e_expr_target;
    { E_Expr.UnOpt (Floor, e) } %prec unopt_prec
  | LOG_E; e = e_expr_target;
    { E_Expr.UnOpt (Log_e, e) } %prec unopt_prec
  | LOG_10; e = e_expr_target;
    { E_Expr.UnOpt (Log_10, e) } %prec unopt_prec
  | RANDOM; e = e_expr_target;
    { E_Expr.UnOpt (Random, e) } %prec unopt_prec
  | SIN; e = e_expr_target;
    { E_Expr.UnOpt (Sin, e) } %prec unopt_prec
  | SQRT; e = e_expr_target;
    { E_Expr.UnOpt (Sqrt, e) } %prec unopt_prec
  | TAN; e = e_expr_target;
    { E_Expr.UnOpt (Tan, e) } %prec unopt_prec
  | OBJ_TO_LIST; e = e_expr_target;
    { E_Expr.UnOpt (ObjToList, e) } %prec unopt_prec
  | SCONCAT; e = e_expr_target;
    { E_Expr.UnOpt (Sconcat, e) } %prec unopt_prec
  | OBJ_FIELDS; e = e_expr_target;
    { E_Expr.UnOpt (ObjFields, e) } %prec unopt_prec
  | PARSE_NUMBER; e = e_expr_target;
    { E_Expr.UnOpt (ParseNumber, e) } %prec unopt_prec
  | PARSE_STRING; e = e_expr_target;
    { E_Expr.UnOpt (ParseString, e) } %prec unopt_prec
  | PARSE_DATE; e = e_expr_target;
    { E_Expr.UnOpt (ParseDate, e) } %prec unopt_prec
  | LREVERSE; e = e_expr_target;
    { E_Expr.UnOpt (LReverse, e) } %prec unopt_prec
  | COSH; e = e_expr_target;
    { E_Expr.UnOpt (Cosh, e) } %prec unopt_prec
  | LOG_2; e = e_expr_target;
    { E_Expr.UnOpt (Log_2, e) } %prec unopt_prec
  | SINH; e = e_expr_target;
    { E_Expr.UnOpt (Sinh, e) } %prec unopt_prec
  | TANH; e = e_expr_target;
    { E_Expr.UnOpt (Tanh, e) } %prec unopt_prec
  | FLOAT64_TO_LE_BYTES; e = e_expr_target;
    { E_Expr.UnOpt (Float64ToLEBytes, e) } %prec unopt_prec 
  | FLOAT64_TO_BE_BYTES; e = e_expr_target;
    { E_Expr.UnOpt (Float64ToBEBytes, e) } %prec unopt_prec 
  | FLOAT32_TO_LE_BYTES; e = e_expr_target;
    { E_Expr.UnOpt (Float32ToLEBytes, e) } %prec unopt_prec 
  | FLOAT32_TO_BE_BYTES; e = e_expr_target;
    { E_Expr.UnOpt (Float32ToBEBytes, e) } %prec unopt_prec 
  | FLOAT64_FROM_LE_BYTES; e = e_expr_target;
    { E_Expr.UnOpt (Float64FromLEBytes, e) } %prec unopt_prec 
  | FLOAT64_FROM_BE_BYTES; e = e_expr_target;
    { E_Expr.UnOpt (Float64FromBEBytes, e) } %prec unopt_prec 
  | FLOAT32_FROM_LE_BYTES; e = e_expr_target;
    { E_Expr.UnOpt (Float32FromLEBytes, e) } %prec unopt_prec 
  | FLOAT32_FROM_BE_BYTES  e = e_expr_target;
    { E_Expr.UnOpt (Float32FromBEBytes, e) } %prec unopt_prec
  | BYTES_TO_STRING  e = e_expr_target;
    { E_Expr.UnOpt (BytesToString, e) } %prec unopt_prec
  | FLOAT_TO_BYTE e = e_expr_target;
    { E_Expr.UnOpt (FloatToByte, e) } %prec unopt_prec
  | ALEN  e = e_expr_target;
    { E_Expr.UnOpt (ArrayLen, e) } %prec unopt_prec
  |  LIST_TO_ARRAY e = e_expr_target;
    { E_Expr.UnOpt (ListToArray, e) } %prec unopt_prec


prefix_binary_op_target:
  | LNTH; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Lnth, e1, e2) }
  | LREM; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (LRem, e1, e2) }
  | LREMNTH; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (LRemNth, e1, e2) }
  | TNTH; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Tnth, e1, e2) }
  | SNTH; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Snth, e1, e2) }
  | SNTH_U; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Snth_u, e1, e2) }
  | SSPLIT; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Ssplit, e1, e2) }
  | LADD; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Ladd, e1, e2) }
  | LPREPEND; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Lprepend, e1, e2) }
  | LCONCAT; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Lconcat, e1, e2) }
  | ATAN_2; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Atan2, e1, e2) }
  | MAX; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Max, e1, e2) }
  | MIN; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Min, e1, e2) }
  | TO_PRECISION; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN; 
    { E_Expr.BinOpt (ToPrecision, e1, e2) }
  | TO_EXPONENTIAL; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN; 
    { E_Expr.BinOpt (ToExponential, e1, e2) }
  | TO_FIXED; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN; 
    { E_Expr.BinOpt (ToFixed, e1, e2) }
  | ARRAY_MAKE; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (ArrayMake, e1, e2) }
  | ANTH; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Anth, e1, e2) }
  | INT_TO_BE_BYTES; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (IntToBEBytes, e1, e2) }
  | INT_FROM_BYTES; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (IntFromBytes, e1, e2) }
  | UINT_FROM_BYTES; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (UintFromBytes, e1, e2) }

prefix_trinary_op_target:
  | SSUBSTR; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; COMMA; e3 = e_expr_target; RPAREN;
    { E_Expr.TriOpt (Ssubstr, e1, e2, e3) }
  | SSUBSTR_U; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; COMMA; e3 = e_expr_target; RPAREN;
    { E_Expr.TriOpt (SsubstrU, e1, e2, e3) }
  | ASET; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; COMMA; e3 = e_expr_target; RPAREN;
    { E_Expr.TriOpt (Aset, e1, e2, e3) }
  | LSET; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; COMMA; e3 = e_expr_target; RPAREN;
    { E_Expr.TriOpt (Lset, e1, e2, e3) }

infix_binary_op_target:
  | e1 = e_expr_target; bop = op_target; e2 = e_expr_target;
    { E_Expr.BinOpt (bop, e1, e2) }
  | e1 = e_expr_target; bop = e_op_target; e2 = e_expr_target;
    { E_Expr.EBinOpt (bop, e1, e2) }

fv_target:
  | f = VAR; COLON; e = e_expr_target;
    { (f, e) }

(* { s1; ...; sn } *)
e_block_target:
  | LBRACE; stmts = separated_list (SEMICOLON, e_stmt_target); RBRACE;
    { if List.length stmts = 1 then List.nth stmts 0
      else E_Stmt.Block stmts @@ at $sloc }

(* s ::= e.f := e | delete e.f | skip | x := e | s1; s2 | if (e) { s1 } else { s2 } | while (e) { s } | return e | return | repeat s until e*)
e_stmt_target:
  | PRINT; e = e_expr_target;
    { E_Stmt.Print e @@ at $sloc }
  | WRAPPER; meta = e_stmt_metadata_target; s = e_block_target;
    { E_Stmt.Wrapper (meta, s) @@ at $sloc }
  | API_ABORT; e = e_expr_target;
    { E_Stmt.Abort e @@ at $sloc }
  | API_ASSUME; e = e_expr_target;
    { E_Stmt.SymStmt (Assume e) @@ at $sloc }
  | ASSERT; e = e_expr_target;
    { E_Stmt.Assert e @@ at $sloc }
  | e1 = e_expr_target; PERIOD; f = VAR; DEFEQ; e2 = e_expr_target;
    { E_Stmt.FieldAssign (e1, E_Expr.Val (Val.Str f), e2) @@ at $sloc }
  | e1 = e_expr_target; LBRACK; f = e_expr_target; RBRACK; DEFEQ; e2 = e_expr_target;
    { E_Stmt.FieldAssign (e1, f, e2) @@ at $sloc }
  | DELETE; e = e_expr_target; PERIOD; f = VAR;
    { E_Stmt.FieldDelete (e, E_Expr.Val (Val.Str f)) @@ at $sloc }
  | DELETE; e = e_expr_target; LBRACK; f = e_expr_target; RBRACK;
    { E_Stmt.FieldDelete (e, f) @@ at $sloc }
  | SKIP;
    { E_Stmt.Skip @@ at $sloc }
  | v = VAR; DEFEQ; e = e_expr_target;
    { E_Stmt.Assign (v, e) @@ at $sloc }
  | v = GVAR; DEFEQ; e = e_expr_target;
    { E_Stmt.GlobAssign (v, e) @@ at $sloc }
  | e_stmt = ifelse_target;
    { e_stmt }
  | IF; LPAREN; e1 = e_expr_target; RPAREN; meta1 = option(e_stmt_metadata_target); s1 = e_block_target;
    es2 = elif_target; ess = list(elif_target); else_stmt = option(final_else_target);
    {
      let meta1' = Option.default [] meta1 in
      let ess' = (e1, s1, meta1')::es2::ess in
      E_Stmt.EIf (ess', else_stmt) @@ at $sloc
    }
  | WHILE; LPAREN; e = e_expr_target; RPAREN; s = e_block_target;
    { E_Stmt.While (e, s) @@ at $sloc }
  | FOREACH; LPAREN; x = VAR; COLON; e = e_expr_target; RPAREN; s = e_block_target;
    { E_Stmt.ForEach (x, e, s, [], None) @@ at $sloc }
  | FOREACH; LPAREN; x = VAR; COLON; e = e_expr_target; RPAREN; meta = e_stmt_metadata_target; var_meta_opt = option(delimited(LBRACK, var_metadata_target, RBRACK)); s = e_block_target;
    { E_Stmt.ForEach (x, e, s, meta, var_meta_opt) @@ at $sloc }
  | RETURN; e = e_expr_target;
    { E_Stmt.Return e @@ at $sloc }
  | RETURN;
    /* { E_Stmt.Return (E_Expr.Val (Val.Void)) } */
    { E_Stmt.Return (E_Expr.Val Val.Null) @@ at $sloc }
  | THROW; e = e_expr_target;
    { E_Stmt.Throw e @@ at $sloc }
  | FAIL; e = e_expr_target;
    { E_Stmt.Fail e @@ at $sloc }
  | e = e_expr_target;
    { E_Stmt.ExprStmt e @@ at $sloc }
  | REPEAT; meta = option(e_stmt_metadata_target); s = e_block_target;
    { E_Stmt.RepeatUntil (s, E_Expr.Val (Val.Bool false), Option.map_default (fun x -> x) [] meta) @@ at $sloc }
  | REPEAT; meta = option(e_stmt_metadata_target); s = e_block_target; UNTIL; e = e_expr_target;
    { E_Stmt.RepeatUntil (s, e, Option.map_default (fun x -> x) [] meta) @@ at $sloc }
  | MATCH; e = e_expr_target; WITH; PIPE; pat_stmts = separated_list (PIPE, pat_stmt_target);
    { E_Stmt.MatchWith (e, pat_stmts) @@ at $sloc }
  | x = VAR; DEFEQ; LAMBDA; LPAREN;  xs = separated_list (COMMA, VAR); RPAREN; LBRACK; ys = separated_list (COMMA, VAR); RBRACK; s = e_block_target;
    { E_Stmt.Lambda (x, fresh_lambda_id_gen (), xs, ys, s) @@ at $sloc }
  | AT_SIGN; m = VAR; LPAREN; es = separated_list (COMMA, e_expr_target); RPAREN;
    { E_Stmt.MacroApply (m, es) @@ at $sloc }
  | SWITCH; LPAREN; e=e_expr_target; RPAREN; meta = option(case_metadata_target); LBRACE; cases = list (switch_case_target); RBRACE
    { let m = Option.default "" meta in
      E_Stmt.Switch(e, cases, None, m) @@ at $sloc }
  | SWITCH; LPAREN; e=e_expr_target; RPAREN; meta = option(case_metadata_target); LBRACE; cases = list (switch_case_target); SDEFAULT; COLON; s = e_stmt_target; RBRACE
    { let m = Option.default "" meta in
      E_Stmt.Switch(e, cases, Some s, m) @@ at $sloc }

switch_case_target:
  | CASE; e = e_expr_target; COLON; s = e_block_target;
    { (e, s) }

case_metadata_target:
  | LBRACK; s = STRING; RBRACK;
    { s }

(* if (e) { s } | if (e) { s } else { s } | if (e) { s } else if (e) { s } *)
ifelse_target:
  | IF; LPAREN; e = e_expr_target; RPAREN; meta_if = option(e_stmt_metadata_target); s1 = e_block_target; ELSE; meta_else = option(e_stmt_metadata_target); s2 = e_block_target;
    {
      let meta_if' = Option.default [] meta_if in
      let meta_else' = Option.default [] meta_else in
      E_Stmt.If (e, s1, Some s2, meta_if', meta_else') @@ at $sloc
    }
  | IF; LPAREN; e = e_expr_target; RPAREN; meta_if = option(e_stmt_metadata_target); s = e_block_target;
    {
      let meta_if' = Option.default [] meta_if in
      E_Stmt.If (e, s, None, meta_if', []) @@ at $sloc
    }

elif_target:
  | ELIF; LPAREN; e = e_expr_target; RPAREN; meta = option(e_stmt_metadata_target); s = e_block_target;
    { (e, s, Option.map_default (fun x -> x) [] meta) }

final_else_target:
  | ELSE; meta = option(e_stmt_metadata_target); s = e_block_target;
    { (s, Option.map_default (fun x -> x) [] meta) }

e_stmt_metadata_target:
  | LBRACK; meta = separated_list (COMMA, STRING); RBRACK;
    { List.map (
        fun (m : string) : E_Stmt.metadata_t ->
          let sep_idx = String.index_opt m ':' in
          match sep_idx with
          | None   -> { where = m; html = "" }
          | Some idx ->
            let where = String.sub m 0 idx in
            let html = String.sub m (idx+1) ((String.length m)-idx-1) in
            { where; html }
      ) meta
    }
(* { p: v | "x" ! None } [ "", ...] -> s | default -> s *)
pat_stmt_target:
  | p = e_pat_target; RIGHT_ARROW; s = e_block_target;
    { (p, s) }

e_pat_target:
  | LBRACE; pn_patv = separated_list (COMMA, e_pat_v_target); RBRACE; meta = option(pat_metadata_target);
    { E_Pat.ObjPat (pn_patv, meta) }
  | DEFAULT;
    { E_Pat.DefaultPat }

pat_metadata_target:
  | LBRACK; meta = separated_list(COMMA, val_target); RBRACK; vars_meta_opt = option(vars_metadata_target);
    {
      let vars_meta = Option.default [] vars_meta_opt in
      E_Pat_Metadata.build_pat_metadata meta vars_meta
    }

e_pat_v_target:
  | pn = VAR; COLON; pv = e_pat_v_pat_target;
    { (pn, pv) }

e_pat_v_pat_target:
  | v = VAR;
    { E_Pat_v.PatVar v }
  | v = val_target;
    { E_Pat_v.PatVal v }
  | LBRACK; RBRACK;
    { E_Pat_v.PatVal (Val.List []) }
  | NONE;
    { E_Pat_v.PatNone }

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
  | LOR     { Log_Or }
  | BITWISE_AND { BitwiseAnd }
  | PIPE { BitwiseOr }
  | BITWISE_XOR { BitwiseXor }
  | SHIFT_LEFT { ShiftLeft }
  | SHIFT_RIGHT { ShiftRight }
  | SHIFT_RIGHT_LOGICAL { ShiftRightLogical }
  | IN_OBJ  { InObj }
  | IN_LIST { InList }
  | POW     { Pow }

%inline e_op_target:
  | SCLAND  { EOper.SCLogAnd }
  | SCLOR   { EOper.SCLogOr }
