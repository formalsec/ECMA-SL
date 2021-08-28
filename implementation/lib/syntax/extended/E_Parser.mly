(* parser-specification file *)


%{
  let fresh_lambda_id_gen = String_Utils.make_fresh_var_generator "__lambda__"
%}

(*
  BEGIN first section - declarations
  - token and type specifications, precedence directives and other output directives
*)
%token SKIP
%token PRINT WRAPPER
%token ASSERT
%token DEFEQ
%token WHILE FOREACH
%token IF ELSE ELIF
%token RETURN
%token SWITCH SDEFAULT
%token NULL
%token FUNCTION
%token MACRO
%token AT_SIGN
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token PERIOD COMMA SEMICOLON COLON
%token DELETE
%token REPEAT UNTIL
%token MATCH WITH RIGHT_ARROW NONE DEFAULT CASE LAMBDA EXTERN
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
%token ABS ACOS ASIN ATAN ATAN_2 CEIL COS EXP FLOOR LOG_E LOG_10 MAX MIN POW RANDOM SIN SQRT TAN PI MAX_VALUE MIN_VALUE COSH LOG_2 SINH TANH
%token PLUS MINUS TIMES DIVIDE MODULO EQUAL GT LT EGT ELT IN_OBJ IN_LIST TO_PRECISION TO_EXPONENTIAL TO_FIXED
%token NOT LLEN LNTH LADD LPREPEND LCONCAT LREVERSE HD TL TLEN TNTH FST SND LREMOVELAST LSORT SLEN SLEN_U SNTH SNTH_U SSUBSTR SSUBSTR_U
%token SCONCAT SSPLIT
%token IMPORT THROW FAIL CATCH
%token TYPEOF INT_TYPE FLT_TYPE BOOL_TYPE STR_TYPE LOC_TYPE
%token LIST_TYPE TUPLE_TYPE NULL_TYPE SYMBOL_TYPE CURRY_TYPE
%token EOF

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

e_prog_e_stmt_target:
  | s = e_block_target; EOF; { s }

e_prog_e_func_target:
  | f = proc_target; EOF; { f }

e_prog_target:
  | imports = list (import_target); macros_funcs = separated_list (SEMICOLON, e_prog_elem_target); EOF;
   {
    let (funcs, macros) = List.split macros_funcs in
    let funcs' = List.concat (List.map (fun o -> Option.map_default (fun x -> [ x ]) [] o) funcs) in
    let macros' = List.concat (List.map (fun o -> Option.map_default (fun x -> [ x ]) [] o) macros) in
    E_Prog.create imports funcs' macros'
   }

import_target:
  | IMPORT; fname = STRING; SEMICOLON;
    { fname }

e_prog_elem_target:
  | f = proc_target;
    { (Some f, None) }
  | m = macro_target;
    { (None, Some m) }

proc_target:
  | FUNCTION; f = VAR; LPAREN; vars = proc_params_target; RPAREN; s = e_block_target;
   { E_Func.create None f vars s }
  | FUNCTION; f = VAR; LPAREN; vars = proc_params_target; RPAREN; meta = metadata_target; vars_meta_opt = option(vars_metadata_target); s = e_block_target;
   {
     let vars_meta = Option.default [] vars_meta_opt in
     let metadata = E_Func_Metadata.build_func_metadata meta vars_meta in
     E_Func.create (Some metadata) f vars s }

proc_params_target:
  | params = separated_list (COMMA, VAR);
    { params }
  /* | params = separated_list (COMMA, VAR); COMMA; LBRACK; fparams = separated_list(COMMA, VAR); RBRACK;
    { params @ fparams } */

macro_target:
  | MACRO; m = VAR; LPAREN; vars = separated_list (COMMA, VAR); RPAREN; s = e_block_target;
   { E_Macro.create m vars s }

metadata_target:
  | LBRACK; meta = separated_list (COMMA, val_target); RBRACK;
    { meta }

vars_metadata_target:
  | LBRACK; meta = separated_list (COMMA, var_metadata_target); RBRACK;
    { meta }

var_metadata_target:
  | meta = STRING;
    {
      let param_alt = String.split_on_char ':' meta in
      match List.length param_alt with
      | 2 -> ( List.nth param_alt 0, List.nth param_alt 1 )
      | _ -> raise (Failure "Invalid function's variables metadata")
    }

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
    (* This replaces helps on fixing errors when parsing some escape characters. *)
    { Val.Str s }
  | s = SYMBOL;
    { Val.Symbol s }
  | l = LOC;
    { Val.Loc l }
  | t = type_target;
    { Val.Type t }


(* e ::= {} | {f:e} | [] | [e] | e.f | e[f] | v | x | -e | e+e | f(e) | (e) *)
e_expr_target:
  | LBRACE; fes = separated_list (COMMA, fv_target); RBRACE;
    { E_Expr.NewObj (fes) }
  | e = e_expr_target; PERIOD; f = VAR;
    { E_Expr.Lookup (e, E_Expr.Val (Str f)) }
  | e = e_expr_target; LBRACK; f = e_expr_target; RBRACK;
    { E_Expr.Lookup (e, f) }
  | v = val_target;
    { E_Expr.Val v }
  | v = VAR;
    { E_Expr.Var v }
  | v = GVAR;
    { E_Expr.GVar v }
  | MAX_VALUE;
    { E_Expr.Const Oper.MAX_VALUE }
  | MIN_VALUE;
    { E_Expr.Const Oper.MIN_VALUE }
  | PI;
    { E_Expr.Const Oper.PI }
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


nary_op_target:
  | LBRACK; es = separated_list (COMMA, e_expr_target); RBRACK;
    { E_Expr.NOpt (Oper.ListExpr, es) }
  | LPAREN; t = tuple_target; RPAREN;
    { E_Expr.NOpt (Oper.TupleExpr, List.rev t) }

prefix_unary_op_target:
  | MINUS; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Neg, e) } %prec unopt_prec
  | NOT; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Not, e) } %prec unopt_prec
  | BITWISE_NOT; e = e_expr_target;
    { E_Expr.UnOpt (Oper.BitwiseNot, e) } %prec unopt_prec
  | LLEN; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ListLen, e) } %prec unopt_prec
  | TLEN; e = e_expr_target;
    { E_Expr.UnOpt (Oper.TupleLen, e) } %prec unopt_prec
  | SLEN; e = e_expr_target;
    { E_Expr.UnOpt (Oper.StringLen, e) } %prec unopt_prec
  | SLEN_U; e = e_expr_target;
    { E_Expr.UnOpt (Oper.StringLenU, e) } %prec unopt_prec
  | TYPEOF; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Typeof, e) } %prec unopt_prec
  | HD; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Head, e) } %prec unopt_prec
  | TL; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Tail, e) } %prec unopt_prec
  | FST; e = e_expr_target;
    { E_Expr.UnOpt (Oper.First, e) } %prec unopt_prec
  | SND; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Second, e) } %prec unopt_prec
  | LREMOVELAST; e = e_expr_target;
    { E_Expr.UnOpt (Oper.LRemoveLast, e) } %prec unopt_prec
  | LSORT; e = e_expr_target;
    { E_Expr.UnOpt (Oper.LSort, e) } %prec unopt_prec
  | INT_TO_FLOAT; e = e_expr_target;
    { E_Expr.UnOpt (Oper.IntToFloat, e) } %prec unopt_prec
  | INT_TO_STRING; e = e_expr_target;
    { E_Expr.UnOpt (Oper.IntToString, e) } %prec unopt_prec
  | INT_TO_FOUR_HEX; e = e_expr_target;
    { E_Expr.UnOpt (Oper.IntToFourHex, e) } %prec unopt_prec
  | HEX_DECODE; e = e_expr_target;
    { E_Expr.UnOpt (Oper.HexDecode, e) } %prec unopt_prec
  | UTF8_DECODE; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Utf8Decode, e) } %prec unopt_prec
  | OCTAL_TO_DECIMAL; e = e_expr_target;
    { E_Expr.UnOpt (Oper.OctalToDecimal, e) } %prec unopt_prec
  | INT_OF_STRING; e = e_expr_target;
    { E_Expr.UnOpt (Oper.IntOfString, e) } %prec unopt_prec
  | INT_OF_FLOAT; e = e_expr_target;
    { E_Expr.UnOpt (Oper.IntOfFloat, e) } %prec unopt_prec
  | FLOAT_TO_STRING; e = e_expr_target;
    { E_Expr.UnOpt (Oper.FloatToString, e) } %prec unopt_prec
  | FLOAT_OF_STRING; e = e_expr_target;
    { E_Expr.UnOpt (Oper.FloatOfString, e) } %prec unopt_prec
  | TO_INT; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ToInt, e) } %prec unopt_prec
  | TO_INT32; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ToInt32, e) } %prec unopt_prec
  | TO_UINT32; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ToUint32, e) } %prec unopt_prec
  | FROM_CHAR_CODE; e = e_expr_target;
    { E_Expr.UnOpt (Oper.FromCharCode, e) } %prec unopt_prec
  | FROM_CHAR_CODE_U; e = e_expr_target;
    { E_Expr.UnOpt (Oper.FromCharCodeU, e) } %prec unopt_prec
  | TO_CHAR_CODE; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ToCharCode, e) } %prec unopt_prec
  | TO_CHAR_CODE_U; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ToCharCodeU, e) } %prec unopt_prec
  | TO_LOWER_CASE; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ToLowerCase, e) } %prec unopt_prec
  | TO_UPPER_CASE; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ToUpperCase, e) } %prec unopt_prec
  | TRIM; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Trim, e) } %prec unopt_prec
  | TO_UINT16; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ToUint16, e) } %prec unopt_prec
  | ABS; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Abs, e) } %prec unopt_prec
  | ACOS; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Acos, e) } %prec unopt_prec
  | ASIN; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Asin, e) } %prec unopt_prec
  | ATAN; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Atan, e) } %prec unopt_prec
  | CEIL; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Ceil, e) } %prec unopt_prec
  | COS; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Cos, e) } %prec unopt_prec
  | EXP; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Exp, e) } %prec unopt_prec
  | FLOOR; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Floor, e) } %prec unopt_prec
  | LOG_E; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Log_e, e) } %prec unopt_prec
  | LOG_10; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Log_10, e) } %prec unopt_prec
  | RANDOM; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Random, e) } %prec unopt_prec
  | SIN; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Sin, e) } %prec unopt_prec
  | SQRT; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Sqrt, e) } %prec unopt_prec
  | TAN; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Tan, e) } %prec unopt_prec
  | OBJ_TO_LIST; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ObjToList, e) } %prec unopt_prec
  | SCONCAT; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Sconcat, e) } %prec unopt_prec
  | OBJ_FIELDS; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ObjFields, e) } %prec unopt_prec
  | PARSE_NUMBER; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ParseNumber, e) } %prec unopt_prec
  | PARSE_STRING; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ParseString, e) } %prec unopt_prec
  | PARSE_DATE; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ParseDate, e) } %prec unopt_prec
  | LREVERSE; e = e_expr_target;
    { E_Expr.UnOpt (Oper.LReverse, e) } %prec unopt_prec
  | COSH; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Cosh, e) } %prec unopt_prec
  | LOG_2; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Log_2, e) } %prec unopt_prec
  | SINH; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Sinh, e) } %prec unopt_prec
  | TANH; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Tanh, e) } %prec unopt_prec


prefix_binary_op_target:
  | LNTH; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Lnth, e1, e2) }
  | TNTH; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Tnth, e1, e2) }
  | SNTH; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Snth, e1, e2) }
  | SNTH_U; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Snth_u, e1, e2) }
  | SSPLIT; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Ssplit, e1, e2) }
  | LADD; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Ladd, e1, e2) }
  | LPREPEND; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Lprepend, e1, e2) }
  | LCONCAT; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Lconcat, e1, e2) }
  | ATAN_2; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Atan2, e1, e2) }
  | MAX; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Max, e1, e2) }
  | MIN; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Min, e1, e2) }
  | TO_PRECISION; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN; 
    { E_Expr.BinOpt (Oper.ToPrecision, e1, e2) }
  | TO_EXPONENTIAL; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN; 
    { E_Expr.BinOpt (Oper.ToExponential, e1, e2) }
  | TO_FIXED; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN; 
    { E_Expr.BinOpt (Oper.ToFixed, e1, e2) }

prefix_trinary_op_target:
  | SSUBSTR; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; COMMA; e3 = e_expr_target; RPAREN;
    { E_Expr.TriOpt (Oper.Ssubstr, e1, e2, e3) }
  | SSUBSTR_U; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; COMMA; e3 = e_expr_target; RPAREN;
    { E_Expr.TriOpt (Oper.SsubstrU, e1, e2, e3) }

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
    { if List.length stmts = 1 then List.nth stmts 0 else E_Stmt.Block stmts }

(* s ::= e.f := e | delete e.f | skip | x := e | s1; s2 | if (e) { s1 } else { s2 } | while (e) { s } | return e | return | repeat s until e*)
e_stmt_target:
  | PRINT; e = e_expr_target;
    { E_Stmt.Print e }
  | WRAPPER; meta = e_stmt_metadata_target; s = e_block_target;
    { E_Stmt.Wrapper (meta, s) }
  | ASSERT; e = e_expr_target;
    { E_Stmt.Assert e }
  | e1 = e_expr_target; PERIOD; f = VAR; DEFEQ; e2 = e_expr_target;
    { E_Stmt.FieldAssign (e1, E_Expr.Val (Str f), e2) }
  | e1 = e_expr_target; LBRACK; f = e_expr_target; RBRACK; DEFEQ; e2 = e_expr_target;
    { E_Stmt.FieldAssign (e1, f, e2) }
  | DELETE; e = e_expr_target; PERIOD; f = VAR;
    { E_Stmt.FieldDelete (e, E_Expr.Val (Str f)) }
  | DELETE; e = e_expr_target; LBRACK; f = e_expr_target; RBRACK;
    { E_Stmt.FieldDelete (e, f) }
  | SKIP;
    { E_Stmt.Skip }
  | v = VAR; DEFEQ; e = e_expr_target;
    { E_Stmt.Assign (v, e) }
  | v = GVAR; DEFEQ; e = e_expr_target;
    { E_Stmt.GlobAssign (v, e) }
  | e_stmt = ifelse_target;
    { e_stmt }
  | IF; LPAREN; e1 = e_expr_target; RPAREN; meta1 = option(e_stmt_metadata_target); s1 = e_block_target;
    es2 = elif_target; ess = list(elif_target); else_stmt = option(final_else_target);
    {
      let meta1' = Option.default [] meta1 in
      let ess' = (e1, s1, meta1')::es2::ess in
      E_Stmt.EIf (ess', else_stmt)
    }
  | WHILE; LPAREN; e = e_expr_target; RPAREN; s = e_block_target;
    { E_Stmt.While (e, s) }
  | FOREACH; LPAREN; x = VAR; COLON; e = e_expr_target; RPAREN; s = e_block_target;
    { E_Stmt.ForEach (x, e, s, [], None) }
  | FOREACH; LPAREN; x = VAR; COLON; e = e_expr_target; RPAREN; meta = e_stmt_metadata_target; var_meta_opt = option(delimited(LBRACK, var_metadata_target, RBRACK)); s = e_block_target;
    { E_Stmt.ForEach (x, e, s, meta, var_meta_opt) }
  | RETURN; e = e_expr_target;
    { E_Stmt.Return e }
  | RETURN;
    /* { E_Stmt.Return (E_Expr.Val (Val.Void)) } */
    { E_Stmt.Return (E_Expr.Val Val.Null) }
  | THROW; e = e_expr_target;
    { E_Stmt.Throw e }
  | FAIL; e = e_expr_target;
    { E_Stmt.Fail e }
  | e = e_expr_target;
    { E_Stmt.ExprStmt e }
  | REPEAT; s = e_block_target;
    { E_Stmt.RepeatUntil (s, E_Expr.Val (Val.Bool false)) }
  | REPEAT; s = e_block_target; UNTIL; e = e_expr_target;
    { E_Stmt.RepeatUntil (s, e) }
  | MATCH; e = e_expr_target; WITH; PIPE; pat_stmts = separated_list (PIPE, pat_stmt_target);
    { E_Stmt.MatchWith (e, pat_stmts) }
  | x = VAR; DEFEQ; LAMBDA; LPAREN;  xs = separated_list (COMMA, VAR); RPAREN; LBRACK; ys = separated_list (COMMA, VAR); RBRACK; s = e_block_target;
    { E_Stmt.Lambda (x, fresh_lambda_id_gen (), xs, ys, s) }
  | AT_SIGN; m = VAR; LPAREN; es = separated_list (COMMA, e_expr_target); RPAREN;
    { E_Stmt.MacroApply (m, es) }
  | SWITCH; LPAREN; e=e_expr_target; RPAREN; meta = option(case_metadata_target); LBRACE; cases = list (switch_case_target); RBRACE
    { let m = Option.default "" meta in
      E_Stmt.Switch(e, cases, None, m) }
  | SWITCH; LPAREN; e=e_expr_target; RPAREN; meta = option(case_metadata_target); LBRACE; cases = list (switch_case_target); SDEFAULT; COLON; s = e_stmt_target; RBRACE
    { let m = Option.default "" meta in
      E_Stmt.Switch(e, cases, Some s, m) }

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
      E_Stmt.If (e, s1, Some s2, meta_if', meta_else')
    }
  | IF; LPAREN; e = e_expr_target; RPAREN; meta_if = option(e_stmt_metadata_target); s = e_block_target;
    {
      let meta_if' = Option.default [] meta_if in
      E_Stmt.If (e, s, None, meta_if', [])
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
          | None   -> { where=m; html="" }
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
  | LOR     { Oper.Log_Or }
  | BITWISE_AND { Oper.BitwiseAnd }
  | PIPE { Oper.BitwiseOr }
  | BITWISE_XOR { Oper.BitwiseXor }
  | SHIFT_LEFT { Oper.ShiftLeft }
  | SHIFT_RIGHT { Oper.ShiftRight }
  | SHIFT_RIGHT_LOGICAL { Oper.ShiftRightLogical }
  | IN_OBJ  { Oper.InObj }
  | IN_LIST { Oper.InList }
  | POW     { Oper.Pow }

%inline e_op_target:
  | SCLAND  { EOper.SCLogAnd }
  | SCLOR   { EOper.SCLogOr }
