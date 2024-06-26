(* ================================= *)
(*            Definitions            *)
(* ================================= *)

%{
  open EslSyntax
  open EslSyntax.Source

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
      real  = true;
    }
%}

(* ========== Typed tokens ========== *)

%token <int>    INT
%token <int>    LOC
%token <float>  FLOAT
%token <string> STRING
%token <bool>   BOOLEAN
%token <string> SYMBOL
%token <string> ID

(* ========== Language tokens ========== *)

%token NULL
%token PRINT DELETE
%token FUNCTION RETURN EXTERN
%token IF ELSE
%token WHILE
%token SWITCH CASE SDEFAULT
%token FAIL ASSERT

(* ========== Symbol tokens ========== *)

%token PERIOD COMMA SEMICOLON COLON
%token DEFEQ
%token ATSIGN HASH
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token LARRBRACK RARRBRACK
%token EOF

(* ========== Operator tokens ========== *)

%token OBJECT_TO_LIST OBJECT_FIELDS
%token OBJECT_MEM

(* ========== Precedence and associativity ========== *)

%left LAND LOR
%left EQ NEQ
%left LT GT LE GE
%left AMPERSAND PIPE CARET SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%left MINUS PLUS
%left TIMES DIVIDE MODULO
%right POW

%nonassoc unopt_prec

(* ========== Entry point ========== *)

%type <Expr.t> entry_expr_target
%type <Stmt.t> entry_stmt_target
%type <Func.t> entry_func_target
%type <Prog.t> entry_prog_target

%start
entry_expr_target
entry_stmt_target
entry_func_target
entry_prog_target



(* ======================================= *)
(*            Grammar and rules            *)
(* ======================================= *)

%%

let entry_expr_target := ~ = expr_target; EOF; <>
let entry_stmt_target := ~ = stmt_target; EOF; <>
let entry_func_target := ~ = func_target; EOF; <>
let entry_prog_target := ~ = prog_target; EOF; <>

(* ==================== Program  ==================== *)

let prog_target := ~ = separated_list(SEMICOLON, func_target); < Prog.create >

(* ==================== Functions ==================== *)

let func_target :=
  | FUNCTION; fn = id_target; LPAREN; pxs = separated_list(COMMA, id_target); RPAREN; s = block_target;
    { Func.create fn (Parsing_helper.Prog.parse_params pxs) s @> at $sloc }

(* ==================== Statements ==================== *)

let block_target :=
  | LBRACE; ss = separated_list (SEMICOLON, stmt_target); RBRACE;
    { Stmt.Block ss @> at $sloc }

let stmt_target :=
  | HASH; s = stmt_target;
    { Stmt.Debug s @> at $sloc }
  | PRINT; e = expr_target;
    { Stmt.Print e @> at $sloc }
  | RETURN;
    { Stmt.Return (Expr.Val (Value.App (`Op "void", [])) @> at $sloc) @> at $sloc }
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
  | SWITCH; LPAREN; e = expr_target; RPAREN; LBRACE;
    css = list(switch_case_target); dflt = switch_default_target?; RBRACE;
    { Stmt.Switch (e, (Parsing_helper.Stmt.parse_switch_cases css), dflt) @> at $sloc }
  | FAIL; e = expr_target;
    { Stmt.Fail e @> at $sloc }
  | ASSERT; e = expr_target;
    { Stmt.Assert e @> at $sloc }

let lookup_target :=
  | PERIOD; fn = id_target;             { Expr.Val (Value.Str fn.it) @> at $sloc }
  | LBRACK; fe = expr_target; RBRACK;   { fe }

let switch_case_target :=
  | CASE; v = val_target; COLON; s = block_target;     { (v @> at $sloc, s) }

let switch_default_target :=
  | SDEFAULT; COLON; s = block_target;                 { s }

(* ==================== Expressions ==================== *)

let expr_target :=
  | LPAREN; ~ = expr_target; RPAREN;
    <>
  | v = val_target;
    { Expr.Val v @> at $sloc }
  | x = ID;
    { Expr.Var x @> at $sloc }
  | unopt = core_unopt_infix; e = expr_target;   %prec unopt_prec
    { Expr.UnOpt (unopt, e) @> at $sloc }
  | unopt = core_unopt_call; e = expr_target;    %prec unopt_prec
    { Expr.UnOpt (unopt, e) @> at $sloc }
  | e1 = expr_target; binopt = core_binopt_infix; e2 = expr_target;
    { Expr.BinOpt (binopt, e1, e2) @> at $sloc }
  | binopt = core_binopt_call; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { Expr.BinOpt (binopt, e1, e2) @> at $sloc }
  | triopt = core_triopt; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; COMMA; e3 = expr_target; RPAREN;
    { Expr.TriOpt (triopt, e1, e2, e3) @> at $sloc }
  | ~ = nopt_target;
    <>
  | LBRACE; fe = expr_target; RBRACE; ATSIGN; LPAREN; es = separated_list(COMMA, expr_target); RPAREN;
    { Expr.Curry (fe, es) @> at $sloc }

let nopt_target :=
  | LARRBRACK; es = separated_list (COMMA, expr_target); RARRBRACK;
    { Expr.NOpt (ArrayExpr, es) @> at $sloc }
  | LBRACK; es = separated_list (COMMA, expr_target); RBRACK;
    { Expr.NOpt (ListExpr, es) @> at $sloc }

(* ==================== Values ==================== *)

let id_target := x = ID; { (x @> at $sloc) }

let val_target :=
  | NULL;                { Value.App (`Op "null", []) }
  | i = INT;             < Value.Int >
  | f = FLOAT;           < Value.Real >
  | s = STRING;          < Value.Str >
  | b = BOOLEAN;         { if b then Value.True else Value.False }
  | l = LOC;             { Value.App (`Op "loc", [Value.Int l])}
  | s = SYMBOL;          { Value.App (`Op "symbol", [Value.Str s])}
