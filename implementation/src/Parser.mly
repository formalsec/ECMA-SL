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
%token <float> FLOAT
%token <int> INT
%token <bool> BOOLEAN
%token <string> VAR
%token <string> STRING
%token LAND LOR
%token PLUS MINUS TIMES DIVIDE EQUAL GT LT EGT ELT IN NOT
%token TYPEOF
%token INT_TYPE
%token FLT_TYPE
%token STR_TYPE
%token BOOL_TYPE
%token EOF

%left LAND LOR
%left GT LT EGT ELT IN
%left PLUS MINUS
%left TIMES DIVIDE
%left EQUAL
%left SEMICOLON

%nonassoc binopt_prec
%nonassoc unopt_prec

%type <Expr.t> prog_expr_target
%type <Stmt.t> prog_stmt_target
%type <Func.t > proc_target
%type <Func.t list> prog_target

%start prog_target prog_expr_target prog_stmt_target
%% (* separator line *)
(* END first section - declarations *)

(*
  BEGIN - second section - grammar and rules
  - specifying the grammar of the language to be parsed, specifying the productions
  - productions are organized into rules, where each rule lists all
    the possible productions for a given nonterminal.
*)

prog_expr_target:
  | e = expr_target; EOF; { e }

prog_stmt_target:
  | s = stmt_target; EOF; { s }

prog_target:
  | funcs = separated_list (SEMICOLON, proc_target); EOF;
   { funcs }

proc_target:
  | FUNCTION; f = VAR; LPAREN; vars = separated_list (COMMA, VAR); RPAREN; LBRACE; s =separated_list (SEMICOLON,stmt_target); RBRACE
   { Func.create f vars s }

(*
  The pipes separate the individual productions, and the curly braces contain a semantic action:
    OCaml code that generates the OCaml value corresponding to the production in question.
  Semantic actions are arbitrary OCaml expressions that are evaluated during parsing
    to produce values that are attached to the nonterminal in the rule.
*)

type_target:
  | INT_TYPE;
    { Type.IntType }
  | FLT_TYPE;
    { Type.FltType }
  | STR_TYPE;
    { Type.StrType }
  | BOOL_TYPE;
    { Type.BoolType }

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
  | t = type_target;
    { Val.Type t }

(* e ::= {} | {f:e} | [] | [e] | e.f | e[f] | v | x | -e | e+e | f(e) | (e) *)
expr_target:
  | LBRACK; es = separated_list (COMMA, expr_target); RBRACK;
    { Expr.NOpt (Oper.ListExpr, es) }
  | v = val_target;
    { Expr.Val v }
  | v = VAR;
    { Expr.Var v }
  | MINUS; e = expr_target;
    { Expr.UnOpt (Oper.Neg, e) } %prec unopt_prec
  | NOT; e = expr_target;
    { Expr.UnOpt (Oper.Not, e) } %prec unopt_prec
  | TYPEOF; e = expr_target;
    { Expr.UnOpt ( Oper.Typeof, e) } %prec unopt_prec
  | e1 = expr_target; bop = op_target; e2 = expr_target;
    { Expr.BinOpt (bop, e1, e2) } %prec binopt_prec
  | LPAREN; e = expr_target; RPAREN;
    { e }


(* s ::= e.f := e | delete e.f | skip | x := e | s1; s2 | if (e) { s1 } else { s2 } | while (e) { s } | return e | return *)
stmt_target:
  | e1 = expr_target; PERIOD; f = VAR; DEFEQ; e2 = expr_target;
    { Stmt.FieldAssign (e1, Expr.Val (Str f), e2) }
  | e1 = expr_target; LBRACK; f = expr_target; RBRACK; DEFEQ; e2 = expr_target;
    { Stmt.FieldAssign (e1, f, e2) }
  | DELETE; e = expr_target; PERIOD; f = VAR;
    { Stmt.FieldDelete (e, Expr.Val (Str f)) }
  | DELETE; e = expr_target; LBRACK; f = expr_target; RBRACK;
    { Stmt.FieldDelete (e, f) }
  | SKIP;
    { Stmt.Skip }
  | v = VAR; DEFEQ; e = expr_target;
    { Stmt.Assign (v, e) }
  | exps_stmts = ifelse_target;
    { exps_stmts }
  | WHILE; LPAREN; e = expr_target; RPAREN; LBRACE; s = stmt_target; RBRACE;
    { Stmt.While (e, s) }
  | RETURN; e = expr_target;
    { Stmt.Return e }
  | RETURN;
    { Stmt.Return (Expr.Val Val.Void) }
  | v=VAR; DEFEQ; f=expr_target; LPAREN;vs= separated_list(COMMA, expr_target);RPAREN;
  {Stmt.AssignCall (v,f,vs)}

  | v=VAR; DEFEQ; e = expr_target; PERIOD; f = VAR;
    { Stmt.AssignAccess (v,e, Expr.Val (Str f)) }
  | v=VAR; DEFEQ;e = expr_target; LBRACK; f = expr_target; RBRACK;
    { Stmt.AssignAccess (v,e, f) }
  | v=VAR; DEFEQ;LBRACE; RBRACE;
    { Stmt.AssignNewObj (v) }


(* if (e) { s } | if (e) {s} else { s } *)
ifelse_target:
  | IF; LPAREN; e = expr_target; RPAREN; LBRACE; s1 = stmt_target; RBRACE; ELSE;LBRACE; s2 = stmt_target; RBRACE;
    { Stmt.If(e, s1, Some s2) }
  | IF; LPAREN; e = expr_target; RPAREN; LBRACE; s = stmt_target; RBRACE;
    { Stmt.If(e, s, None) }

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
  | LAND { Oper.Log_And }
  | LOR  { Oper.Log_Or }
  | IN      { Oper.InObj }
