(* parser-specification file *)

(*
  BEGIN first section - declarations
  - token and type specifications, precedence directives and other output directives
*)
%token SKIP
%token PRINT
%token ASSERT
%token DEFEQ
%token WHILE
%token IF ELSE
%token RETURN
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
%token MATCH WITH RIGHT_ARROW NONE DEFAULT
%token <float> FLOAT
%token <int> INT
%token <bool> BOOLEAN
%token <string> VAR
%token <string> GVAR
%token <string> STRING
%token <string> SYMBOL
%token <string> LOC
%token LAND LOR SCLAND SCLOR
%token INT_TO_FLOAT INT_TO_STRING INT_OF_STRING FLOAT_OF_STRING FLOAT_TO_STRING OBJ_TO_LIST OBJ_FIELDS INT_OF_FLOAT
%token BITWISE_NOT BITWISE_AND PIPE BITWISE_XOR SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%token TO_INT TO_INT32 TO_UINT32 TO_UINT16 FLOOR FROM_CHAR_CODE TO_CHAR_CODE TO_LOWER_CASE TO_UPPER_CASE TRIM
%token PLUS MINUS TIMES DIVIDE MODULO EQUAL GT LT EGT ELT IN_OBJ IN_LIST
%token NOT LLEN LNTH LADD LPREPEND LCONCAT HD TL TLEN TNTH FST SND SLEN SNTH
%token SCONCAT
%token IMPORT THROW
%token TYPEOF INT_TYPE FLT_TYPE BOOL_TYPE STR_TYPE LOC_TYPE
%token LIST_TYPE TUPLE_TYPE NULL_TYPE SYMBOL_TYPE
%token EOF

%left SCLAND SCLOR LAND LOR BITWISE_AND PIPE BITWISE_XOR SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%left GT LT EGT ELT IN_OBJ IN_LIST
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left EQUAL

%nonassoc binopt_prec
%nonassoc unopt_prec
%nonassoc PERIOD LBRACK

%type <E_Expr.t> e_prog_e_expr_target
%type <E_Stmt.t> e_prog_e_stmt_target
%type <E_Prog.t> e_prog_target

%start e_prog_target e_prog_e_expr_target e_prog_e_stmt_target
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
  | FUNCTION; f = VAR; LPAREN; vars = separated_list (COMMA, VAR); RPAREN; s = e_block_target;
   { E_Func.create f vars s }

macro_target:
  | MACRO; m = VAR; LPAREN; vars = separated_list (COMMA, VAR); RPAREN; s = e_block_target;
   { E_Macro.create m vars s }

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
    { let s' = Str.global_replace (Str.regexp "\\") "\\\\\\\\" s in
      let s'' = Str.global_replace (Str.regexp "\"") "\\\"" s' in
      Val.Str s'' }
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
  | f = VAR; LPAREN; es = separated_list (COMMA, e_expr_target); RPAREN;
    { E_Expr.Call (E_Expr.Val (Val.Str f), es) }
  | LBRACE; f = e_expr_target; RBRACE; LPAREN; es = separated_list (COMMA, e_expr_target); RPAREN;
    { E_Expr.Call (f, es) }
  | LPAREN; e = e_expr_target; RPAREN;
    { e }
  | nary_op_expr = nary_op_target;
    { nary_op_expr }
  | pre_un_op_expr = prefix_unary_op_target;
    { pre_un_op_expr }
  | pre_bin_op_expr = prefix_binary_op_target;
    { pre_bin_op_expr }
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
  | INT_TO_FLOAT; e = e_expr_target;
    { E_Expr.UnOpt (Oper.IntToFloat, e) } %prec unopt_prec
  | INT_TO_STRING; e = e_expr_target;
    { E_Expr.UnOpt (Oper.IntToString, e) } %prec unopt_prec
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
  | TO_CHAR_CODE; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ToCharCode, e) } %prec unopt_prec
  | TO_LOWER_CASE; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ToLowerCase, e) } %prec unopt_prec
  | TO_UPPER_CASE; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ToUpperCase, e) } %prec unopt_prec
  | TRIM; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Trim, e) } %prec unopt_prec
  | FLOOR; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Floor, e) } %prec unopt_prec
  | TO_UINT16; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ToUint16, e) } %prec unopt_prec
  | OBJ_TO_LIST; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ObjToList, e) } %prec unopt_prec
  | SCONCAT; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Sconcat, e) } %prec unopt_prec
  | OBJ_FIELDS; e = e_expr_target;
    { E_Expr.UnOpt (Oper.ObjFields, e) } %prec unopt_prec


prefix_binary_op_target:
  | LNTH; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Lnth, e1, e2) }
  | TNTH; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Tnth, e1, e2) }
  | SNTH; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Snth, e1, e2) }
  | LADD; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Ladd, e1, e2) }
  | LPREPEND; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Lprepend, e1, e2) }
  | LCONCAT; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Lconcat, e1, e2) }

infix_binary_op_target:
  | e1 = e_expr_target; bop = op_target; e2 = e_expr_target;
    { E_Expr.BinOpt (bop, e1, e2) } %prec binopt_prec
  | e1 = e_expr_target; bop = e_op_target; e2 = e_expr_target;
     { E_Expr.EBinOpt (bop, e1, e2) } %prec binopt_prec

fv_target:
  | f = VAR; COLON; e = e_expr_target;
    { (f, e) }

(* { s1; ...; sn } *)
e_block_target:
  | LBRACE; stmts = separated_list (SEMICOLON, e_stmt_target); RBRACE;
    { E_Stmt.Block stmts }

(* s ::= e.f := e | delete e.f | skip | x := e | s1; s2 | if (e) { s1 } else { s2 } | while (e) { s } | return e | return | repeat s until e*)
e_stmt_target:
  | PRINT; e = e_expr_target;
    { E_Stmt.Print e }
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
  | WHILE; LPAREN; e = e_expr_target; RPAREN; s = e_block_target;
    { E_Stmt.While (e, s) }
  | RETURN; e = e_expr_target;
    { E_Stmt.Return e }
  | RETURN;
    { E_Stmt.Return (E_Expr.Val Val.Void) }
  | THROW; e = e_expr_target;
    { E_Stmt.Throw e }
  | e = e_expr_target;
    { E_Stmt.ExprStmt e }
  | REPEAT; s = e_block_target;
    { E_Stmt.RepeatUntil (s, E_Expr.Val (Val.Bool false)) }
  | REPEAT; s = e_block_target; UNTIL; e = e_expr_target;
    { E_Stmt.RepeatUntil (s, e) }
  | MATCH; e = e_expr_target; WITH; PIPE; pat_stmts = separated_list (PIPE, pat_stmt_target);
    { E_Stmt.MatchWith (e, pat_stmts) }
  | AT_SIGN; m = VAR; LPAREN; es = separated_list (COMMA, e_expr_target); RPAREN;
    { E_Stmt.MacroApply (m, es) }

(* if (e) { s } | if (e) { s } else { s } *)
ifelse_target:
  | IF; LPAREN; e = e_expr_target; RPAREN; s1 = e_block_target; ELSE; s2 = e_block_target;
    { E_Stmt.If (e, s1, Some s2) }
  | IF; LPAREN; e = e_expr_target; RPAREN; s = e_block_target;
    { E_Stmt.If (e, s, None) }

(* { p: v | "x" ! None } -> s | default -> s *)
pat_stmt_target:
  | p = e_pat_target; RIGHT_ARROW; s = e_block_target;
    { (p, s) }

e_pat_target:
  | LBRACE; pn_patv = separated_list (COMMA, e_pat_v_target); RBRACE;
    { E_Pat.ObjPat pn_patv }
  | DEFAULT;
    { E_Pat.DefaultPat }

e_pat_v_target:
  | pn = VAR; COLON; v = VAR;
    { (pn, E_Pat_v.PatVar v) }
  | pn = VAR; COLON; v = val_target;
    { (pn, E_Pat_v.PatVal v) }
  | pn = VAR; COLON; NONE;
    { (pn, E_Pat_v.PatNone) }

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
  | LOR     { Oper.Log_Or }
  | BITWISE_AND { Oper.BitwiseAnd }
  | PIPE { Oper.BitwiseOr }
  | BITWISE_XOR { Oper.BitwiseXor }
  | SHIFT_LEFT { Oper.ShiftLeft }
  | SHIFT_RIGHT { Oper.ShiftRight }
  | SHIFT_RIGHT_LOGICAL { Oper.ShiftRightLogical }
  | IN_OBJ  { Oper.InObj }
  | IN_LIST { Oper.InList }

e_op_target:
  | SCLAND  { EOper.SCLogAnd }
  | SCLOR   { EOper.SCLogOr }
