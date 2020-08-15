(* parser-specification file *)

(*
  BEGIN first section - declarations
  - token and type specifications, precedence directives and other output directives
*)
%token SKIP
%token DEFEQ
%token WHILE
%token IF ELSE
%token RETURN
%token UNDEFINED
%token NULL
%token FUNCTION
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token PERIOD COMMA SEMICOLON COLON
%token DELETE
%token REPEAT UNTIL
%token MATCH WITH PIPE RIGHT_ARROW NONE DEFAULT
%token <float> FLOAT
%token <int> INT
%token <bool> BOOLEAN
%token <string> VAR
%token <string> STRING
%token LAND LOR
%token PLUS MINUS TIMES DIVIDE EQUAL GT LT EGT ELT IN
%token NOT LEN LNTH
%token IMPORT
%token EOF

%left LAND LOR
%left GT LT EGT ELT IN
%left PLUS MINUS
%left TIMES DIVIDE
%left EQUAL

%nonassoc binopt_prec
%nonassoc unopt_prec

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
  | imports = list (import_target); funcs = separated_list (SEMICOLON, proc_target); EOF;
   { E_Prog.create imports funcs }

import_target:
  | IMPORT; fname = STRING; SEMICOLON;
    { let len = String.length fname in
      let sub = String.sub fname 1 (len - 2) in
      sub }

proc_target:
  | FUNCTION; f = VAR; LPAREN; vars = separated_list (COMMA, VAR); RPAREN; s = e_block_target;
   { E_Func.create f vars s }

(*
  The pipes separate the individual productions, and the curly braces contain a semantic action:
    OCaml code that generates the OCaml value corresponding to the production in question.
  Semantic actions are arbitrary OCaml expressions that are evaluated during parsing
    to produce values that are attached to the nonterminal in the rule.
*)

/* tuple_target:
  | vs = separated_nonempty_list (COMMA, val_target);
    { vs } */

(* v ::= f | i | b | s *)
val_target:
  | UNDEFINED;
    { Val.Undef }
  | NULL;
    { Val.Null }
  | f = FLOAT;
    { Val.Flt f }
  | i = INT;
    { Val.Int i }
  | b = BOOLEAN;
    { Val.Bool b }
  | s = STRING;
    { let len = String.length s in
      let sub = String.sub s 1 (len - 2) in
      Val.Str sub } (* Remove the double-quote characters from the parsed string *)
  /* | LPAREN; t = tuple_target; RPAREN;
    { Val.Tuple t } */

(* e ::= {} | {f:e} | [] | [e] | e.f | e[f] | v | x | -e | e+e | f(e) | (e) *)
e_expr_target:
  | LBRACE; fes = separated_list (COMMA, fv_target); RBRACE;
    { E_Expr.NewObj (fes) }
  | LBRACK; es = separated_list (COMMA, e_expr_target); RBRACK;
    { E_Expr.NOpt (Oper.ListExpr, es) }
  | e = e_expr_target; PERIOD; f = VAR;
    { E_Expr.Access (e, E_Expr.Val (Str f)) }
  | e = e_expr_target; LBRACK; f = e_expr_target; RBRACK;
    { E_Expr.Access (e, f) }
  | v = val_target;
    { E_Expr.Val v }
  | v = VAR;
    { E_Expr.Var v }
  | f = e_expr_target; LPAREN; es = separated_list (COMMA, e_expr_target); RPAREN;
    { E_Expr.Call (f, es) }
  | LPAREN; e = e_expr_target; RPAREN;
    { e }
  | pre_un_op_expr = prefix_unary_op_target;
    { pre_un_op_expr }
  | pre_bin_op_expr = prefix_binary_op_target;
    { pre_bin_op_expr }
  | in_bin_op_expr = infix_binary_op_target;
    { in_bin_op_expr }

prefix_unary_op_target:
  | MINUS; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Neg, e) } %prec unopt_prec
  | NOT; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Not, e) } %prec unopt_prec
  | LEN; e = e_expr_target;
    { E_Expr.UnOpt (Oper.Len, e) } %prec unopt_prec

prefix_binary_op_target:
  | LNTH; LPAREN; e1 = e_expr_target; COMMA; e2 = e_expr_target; RPAREN;
    { E_Expr.BinOpt (Oper.Lnth, e1, e2) }

infix_binary_op_target:
  | e1 = e_expr_target; bop = op_target; e2 = e_expr_target;
    { E_Expr.BinOpt (bop, e1, e2) } %prec binopt_prec

fv_target:
  | f = VAR; COLON; e = e_expr_target;
    { (f, e) }

(* { s1; ...; sn } *)
e_block_target:
  | LBRACE; stmts = separated_list (SEMICOLON, e_stmt_target); RBRACE;
    { E_Stmt.Block stmts }

(* s ::= e.f := e | delete e.f | skip | x := e | s1; s2 | if (e) { s1 } else { s2 } | while (e) { s } | return e | return | repeat s until e*)
e_stmt_target:
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
  | e_stmt = ifelse_target;
    { e_stmt }
  | WHILE; LPAREN; e = e_expr_target; RPAREN; s = e_block_target;
    { E_Stmt.While (e, s) }
  | RETURN; e = e_expr_target;
    { E_Stmt.Return e }
  | RETURN;
    { E_Stmt.Return (E_Expr.Val Val.Void) }
  | e = e_expr_target;
    { E_Stmt.ExprStmt e }
  | REPEAT; s = e_block_target;
    { E_Stmt.RepeatUntil (s, E_Expr.Val (Val.Bool true)) }
  | REPEAT; s = e_block_target; UNTIL; e = e_expr_target;
    { E_Stmt.RepeatUntil (s, e) }
  | MATCH; e = e_expr_target; WITH; PIPE; pat_stmts = separated_list (PIPE, pat_stmt_target);
    { E_Stmt.MatchWith (e, pat_stmts) }

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
  | EQUAL   { Oper.Equal }
  | GT      { Oper.Gt }
  | LT      { Oper.Lt }
  | EGT     { Oper.Egt }
  | ELT     { Oper.Elt }
  | LAND    { Oper.Log_And }
  | LOR     { Oper.Log_Or }
  | IN      { Oper.InObj }

