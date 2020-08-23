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
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token PERIOD COMMA SEMICOLON
%token DELETE
%token <float> FLOAT
%token <int> INT
%token <bool> BOOLEAN
%token <string> VAR
%token <string> STRING
%token <string> SYMBOL
%token LAND LOR
%token PLUS MINUS TIMES DIVIDE EQUAL GT LT EGT ELT IN
%token NOT LEN LNTH HD TL
%token TYPEOF UNDEF_TYPE NULL_TYPE BOOL_TYPE STR_TYPE NUMBER_TYPE OBJ_TYPE REFERENCE_TYPE
%token LIST_TYPE COMPLETION_TYPE ENVIRONMENT_RECORD_TYPE
%token EOF

%left LAND LOR
%left GT LT EGT ELT
%left PLUS MINUS
%left TIMES DIVIDE
%left EQUAL


%nonassoc binopt_prec
%nonassoc unopt_prec


%type <Func.t > proc_target
%type <Func.t list> prog_target

%start prog_target
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

type_target:
  | UNDEF_TYPE;
    { print_string ">UNDEF_TYPE\n";Type.UndefType }
  | NULL_TYPE;
    { print_string ">NULL_TYPE\n";Type.NullType }
  | BOOL_TYPE
    { print_string ">BOOL_TYPE\n";Type.BoolType }
  | STR_TYPE
    { print_string ">STR_TYPE\n";Type.StrType }
  | NUMBER_TYPE;
    { print_string ">NUMBER_TYPE\n";Type.NumberType }
  | OBJ_TYPE;
    { print_string ">OBJ_TYPE\n";Type.ObjType }
  | REFERENCE_TYPE;
    { print_string ">REFERENCE_TYPE\n";Type.ReferenceType }
  | LIST_TYPE;
    { print_string ">LIST_TYPE\n";Type.ListType }
  | COMPLETION_TYPE;
    { print_string ">COMPLETION_TYPE\n";Type.CompletionType }
  | ENVIRONMENT_RECORD_TYPE;
    { print_string ">ENVIRONMENT_RECORD_TYPE\n";Type.EnvironmentRecordType }

tuple_target:
  | v1 = expr_target; COMMA; v2 = expr_target;
    { [v2; v1] }
  | vs = tuple_target; COMMA; v = expr_target;
    { v :: vs }

(* v ::= f | i | b | s *)
val_target:
  | NULL;
    { print_string ">NULL\n";Val.Null }
  | f = FLOAT;
    { print_string ">FLOAT\n";Val.Flt f }
  | i = INT;
    { print_string ">INT\n";Val.Int i }
  | b = BOOLEAN;
    { print_string ">BOOL\n";Val.Bool b }
  | s = STRING;
    { let len = String.length s in
      let sub = String.sub s 1 (len - 2) in
      print_string ">STR\n";Val.Str sub } (* Remove the double-quote characters from the parsed string *)
  | t = type_target;
    { print_string ">TYPE \n";Val.Type t }
  | s = SYMBOL;
    { let len = String.length s in
      let sub = String.sub s 1 (len - 1) in
      print_string ">SYMBOL\n";Val.Symbol sub } (* Remove the double-quote characters from the parsed string *)

(* e ::= {} | {f:e} | [] | [e] | e.f | e[f] | v | x | -e | e+e | f(e) | (e) *)
expr_target:
  | LBRACK; es = separated_list (COMMA, expr_target); RBRACK;
    { print_string ">NOP\n";Expr.NOpt (Oper.ListExpr, es) }
  | LPAREN; t = tuple_target; RPAREN;
    { Expr.NOpt (Oper.TupleExpr, List.rev t) }
  | v = val_target;
    { print_string ">VAL\n"; Expr.Val v }
  | v = VAR;
    { print_string ">VAR\n";  Expr.Var v }
  | MINUS; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.Neg, e) } %prec unopt_prec
  | NOT; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.Not, e) } %prec unopt_prec
  | LEN; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.Len, e) } %prec unopt_prec
  | TYPEOF; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.Typeof, e) } %prec unopt_prec
  | HD; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.Head, e) } %prec unopt_prec
  | TL; e = expr_target;
    { print_string ">UNOP\n"; Expr.UnOpt (Oper.Tail, e) } %prec unopt_prec
  | e1 = expr_target; bop = op_target; e2 = expr_target;
    { print_string ">BINOP\n";Expr.BinOpt (bop, e1, e2) } %prec binopt_prec
  | LNTH; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    {  print_string ">BINOP\n";Expr.BinOpt (Oper.Lnth, e1, e2) }
  | LPAREN; e = expr_target; RPAREN;
    { print_string ">PAREN\n";e }

stmt_block:
| s= separated_list (SEMICOLON, stmt_target);
{
    print_string ">BLOCK\n";Stmt.Block s
}
(* s ::= e.f := e | delete e.f | skip | x := e | s1; s2 | if (e) { s1 } else { s2 } | while (e) { s } | return e | return *)
stmt_target:
  | PRINT; e = expr_target;
    { Stmt.Print e }
  | e1 = expr_target; PERIOD; f = VAR; DEFEQ; e2 = expr_target;
    { print_string ">FIELDASSIGN\n";  Stmt.FieldAssign (e1, Expr.Val (Str f), e2) }
  | e1 = expr_target; LBRACK; f = expr_target; RBRACK; DEFEQ; e2 = expr_target;
    { print_string ">FIELDASSIGN\n";Stmt.FieldAssign (e1, f, e2) }
  | DELETE; e = expr_target; PERIOD; f = VAR;
    { print_string ">FIELDDELETE\n"; Stmt.FieldDelete (e, Expr.Val (Str f)) }
  | DELETE; e = expr_target; LBRACK; f = expr_target; RBRACK;
    {print_string ">FIELDDELETE\n";   Stmt.FieldDelete (e, f) }
  | SKIP;
    { print_string ">SKIP\n";Stmt.Skip }
  | v = VAR; DEFEQ; e = expr_target;
    { print_string ">ASSIGN\n";Stmt.Assign (v, e) }
  | exps_stmts = ifelse_target;
    { exps_stmts }
  | WHILE; LPAREN; e = expr_target; RPAREN; LBRACE; s = stmt_block; RBRACE;
    { print_string ">WHILE\n";Stmt.While (e, s) }
  | RETURN; e = expr_target;
    { print_string ">RETURN\n";Stmt.Return e }
  | RETURN;
    { print_string ">RETURN\n";Stmt.Return (Expr.Val Val.Void) }
  | v=VAR; DEFEQ; f=expr_target; LPAREN;vs= separated_list(COMMA, expr_target);RPAREN;
  {print_string ">ASSIGNCALL\n";Stmt.AssignCall (v,f,vs)}
  | v=VAR; DEFEQ; e1=expr_target; IN; e2= expr_target;
  {print_string ">ASSIGNINOBJCHECK\n";Stmt.AssignInObjCheck (v,e1,e2)}

  | v=VAR; DEFEQ; e = expr_target; PERIOD; f = VAR;
    { print_string ">ASSIGNACCESS\n";Stmt.FieldLookup (v,e, Expr.Val (Str f)) }
  | v=VAR; DEFEQ;e = expr_target; LBRACK; f = expr_target; RBRACK;
    { print_string ">ASSIGNACCESS\n";Stmt.FieldLookup (v,e, f) }
  | v=VAR; DEFEQ;LBRACE; RBRACE;
    { print_string ">ASSIGNNEWOBJ\n";Stmt.AssignNewObj (v) }



(* if (e) { s } | if (e) {s} else { s } *)
ifelse_target:
  | IF; LPAREN; e = expr_target; RPAREN; LBRACE; s1 = stmt_block; RBRACE; ELSE;LBRACE; s2 = stmt_block; RBRACE;
    { print_string ">IF\n";Stmt.If(e, s1, Some s2) }
  | IF; LPAREN; e = expr_target; RPAREN; LBRACE; s = stmt_block; RBRACE;
    { print_string ">IF\n";Stmt.If(e, s, None) }

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
