(* ================================= *)
(*            Definitions            *)
(* ================================= *)

%{
  open Source
  open Operator

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

  let fresh_lambda_id_gen = Utils.make_name_generator "__lambda__"

%}

(* ========== Typed tokens ========== *)

%token <int>    INT
%token <float>  FLOAT
%token <string> STRING
%token <bool>   BOOLEAN
%token <string> SYMBOL
%token <string> LOC
%token <string> ID
%token <string> GID

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
%token QUESTION
%token SCLAND SCLOR
%token RIGHT_ARROW
%token EOF

(* ========== Operator tokens ========== *)

%token MAX_VALUE MIN_VALUE PI

%token ITE

%token OBJECT_TO_LIST OBJECT_FIELDS
%token OBJECT_MEM

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
%left MINUS PLUS
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

let entry_expr_target := ~ = expr_target; EOF; <>

let entry_stmt_target := ~ = stmt_target; EOF; <>

let entry_func_target := ~ = func_target; EOF; <>

let entry_prog_target := ~ = prog_target; EOF; <>

(* ==================== Program  ==================== *)

let prog_target :=
  | imports = import_target*; p_els = separated_list(SEMICOLON?, prog_element_target);
    { EProg.Parser.parse_prog imports p_els }

let import_target := IMPORT; ~ = str_id_target; SEMICOLON; <>

let prog_element_target :=
  | ~ = tdef_target;    < EProg.Parser.parse_tdef >
  | ~ = func_target;    < EProg.Parser.parse_func >
  | ~ = macro_target;   < EProg.Parser.parse_macro >

(* ==================== Type definitions ==================== *)

let tdef_target :=
  | TYPEDEF; tn = id_target; DEFEQ; tv = type_target;
    { EType.tdef_create tn tv }

(* ==================== Functions ==================== *)

let func_target :=
  | FUNCTION; fn = id_target; LPAREN; pxs = separated_list(COMMA, param_target); RPAREN;
    tret = tannot_target?; s = block_target;
    { EFunc.create fn pxs tret s None @> at $sloc }
  | FUNCTION; fn = id_target; LPAREN; pxs = separated_list(COMMA, param_target); RPAREN;
    vals_meta = delimited(LBRACK, vals_metadata_target, RBRACK); vars_meta = vars_opt_metadata_target;
    tret = tannot_target?; s = block_target;
    { EFunc.create fn pxs tret s (Some (EFunc_metadata.build_func_metadata vals_meta vars_meta)) @> at $sloc }

let param_target := ~ = id_target; ~ = tannot_target?; <>

(* ==================== Macros ==================== *)

let macro_target :=
  | MACRO; mn = id_target; LPAREN; pxs = separated_list(COMMA, id_target); RPAREN; s = block_target;
   { EMacro.create mn pxs s @> at $sloc }

(* ==================== Statements ==================== *)

let block_target :=
  | LBRACE; ss = separated_list (SEMICOLON, stmt_target); RBRACE;
    { EStmt.Block ss @> at $sloc }

let stmt_target :=
  | HASH; s = stmt_target;
    { EStmt.Debug s @> at $sloc }
  | PRINT; e = expr_target;
    { EStmt.Print e @> at $sloc }
  | RETURN;
    { EStmt.Return (EExpr.Val Val.Void @> at $sloc) @> at $sloc }
  | RETURN; e = expr_target;
    { EStmt.Return e @> at $sloc }
  | e = expr_target;
    { EStmt.ExprStmt e @> at $sloc }
  | x = id_target; t = option(tannot_target); DEFEQ; e = expr_target;
    { EStmt.Assign (x, t, e) @> at $sloc }
  | x = gid_target; DEFEQ; e = expr_target;
    { EStmt.GAssign (x, e) @> at $sloc }
  | oe = expr_target; fe = lookup_target; DEFEQ; e = expr_target;
    { EStmt.FieldAssign (oe, fe, e) @> at $sloc }
  | DELETE; oe = expr_target; fe = lookup_target;
    { EStmt.FieldDelete (oe, fe) @> at $sloc }
  | ifcs = if_target; elifcss = list(elif_target); elsecs = else_target?;
    { EStmt.If (ifcs :: elifcss, elsecs) @> at $sloc }
  | WHILE; LPAREN; e = expr_target; RPAREN; s = block_target;
    { EStmt.While (e, s) @> at $sloc }
  | FOREACH; LPAREN; x = id_target; COLON; e = expr_target; RPAREN; s = block_target;
    { EStmt.ForEach (x, e, s, [], None) @> at $sloc }
  | FOREACH; LPAREN; x = id_target; COLON; e = expr_target; RPAREN;
    meta = delimited(LBRACK, stmt_metadata_target, RBRACK);
    var_meta = var_opt_metadata_target; s = block_target;
    { EStmt.ForEach (x, e, s, meta, var_meta) @> at $sloc }
  | REPEAT; meta = stmt_opt_metadata_target; s = block_target; until = until_target?;
    { EStmt.RepeatUntil (s, until, meta) @> at $sloc }
  | SWITCH; LPAREN; e = expr_target; RPAREN; meta = str_opt_metadata_target; LBRACE;
    css = list(switch_case_target); dflt = switch_default_target?; RBRACE;
    { EStmt.Switch (e, css, dflt, meta) @> at $sloc }
  | MATCH; e = expr_target; dsc = match_discrm_target?; WITH;
    PIPE; css = separated_list(PIPE, match_case_target);
    { EStmt.MatchWith (e, dsc, css) @> at $sloc }
  | x = id_target; tannot_target?; DEFEQ; LAMBDA; LPAREN; pxs = separated_list(COMMA, id_target); RPAREN;
    LBRACK; ctxvars = separated_list(COMMA, id_target); RBRACK; s = block_target;
    { EStmt.Lambda (x, fresh_lambda_id_gen (), pxs, ctxvars, s) @> at $sloc }
  | ATSIGN; mn = id_target; LPAREN; es = separated_list(COMMA, expr_target); RPAREN;
    { EStmt.MacroApply (mn, es) @> at $sloc }
  | THROW; e = expr_target;
    { EStmt.Throw e @> at $sloc }
  | FAIL; e = expr_target;
    { EStmt.Fail e @> at $sloc }
  | ASSERT; e = expr_target;
    { EStmt.Assert e @> at $sloc }
  | WRAPPER; meta = stmt_opt_metadata_target; s = block_target;
    { EStmt.Wrapper (meta, s) @> at $sloc }

let if_target :=
  | IF; LPAREN; e = expr_target; RPAREN; meta = stmt_opt_metadata_target; s = block_target;
    { (e, s, meta) }

let elif_target :=
  | ELIF; LPAREN; e = expr_target; RPAREN; meta = stmt_opt_metadata_target; s = block_target;
    { (e, s, meta) }

let else_target :=
  | ELSE; meta = stmt_opt_metadata_target; s = block_target;
    { (s, meta) }

let until_target := UNTIL; ~ = expr_target; <>

let switch_case_target := CASE; ~ = expr_target; COLON; ~ = block_target; <>

let switch_default_target := SDEFAULT; COLON; ~ = block_target; <>

let match_discrm_target := COLON; ~ = id_target; <>

let match_case_target := ~ = pattern_target; RIGHT_ARROW; ~ = block_target; <>

(* ==================== Patterns ==================== *)

let pattern_target :=
  | LBRACE; pbs = separated_nonempty_list(COMMA, pattern_binding_target); RBRACE;
    { EPat.ObjPat (pbs, None) @> at $sloc }
  | LBRACE; pbs = separated_nonempty_list(COMMA, pattern_binding_target); RBRACE;
    vals_meta = delimited(LBRACK, vals_metadata_target, RBRACK); vars_meta = vars_opt_metadata_target;
    { EPat.ObjPat (pbs, (Some (EPat_metadata.build_pat_metadata vals_meta vars_meta))) @> at $sloc }
  | DEFAULT;
    { EPat.DefaultPat @> at $sloc }

let pattern_binding_target :=
  | ~ = id_target; COLON; ~ = pattern_value_target; <>
  | ~ = str_id_target; COLON; ~ = pattern_value_target; <>

let pattern_value_target :=
  | x = id_target;        { EPat.PatVar x.it @> at $sloc }
  | v = val_target;       { EPat.PatVal v @> at $sloc }
  | LBRACK; RBRACK;       { EPat.PatVal (Val.List []) @> at $sloc }
  | NONE;                 { EPat.PatNone @> at $sloc }

(* ==================== Expressions ==================== *)

let expr_target :=
  | LPAREN; e = expr_target; RPAREN;
    <>
  | v = val_target;
    { EExpr.Val v @> at $sloc }
  | x = id_target;
    { EExpr.Var x.it @> at $sloc }
  | x = gid_target;
    { EExpr.GVar x.it @> at $sloc }
  | const = const_target;
    { EExpr.Const const @> at $sloc }
  | unopt = core_unopt_infix; e = expr_target;    %prec unopt_prec
    { EExpr.UnOpt (unopt, e) @> at $sloc }
  | unopt = unopt_call_target; e = expr_target;   %prec unopt_prec
    { EExpr.UnOpt (unopt, e) @> at $sloc }
  | e1 = expr_target; binopt = binopt_infix_target; e2 = expr_target;
    { EExpr.BinOpt (binopt, e1, e2) @> at $sloc }
  | binopt = core_binopt_call; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; RPAREN;
    { EExpr.BinOpt (binopt, e1, e2) @> at $sloc }
  | triopt = triopt_call_target; LPAREN; e1 = expr_target; COMMA; e2 = expr_target; COMMA; e3 = expr_target; RPAREN;
    { EExpr.TriOpt (triopt, e1, e2, e3) @> at $sloc }
  | ~ = nopt_target;
    <>
  | fn = id_target; LPAREN; es = separated_list(COMMA, expr_target); RPAREN; ferr = catch_target?;
    { EExpr.Call (EExpr.Val (Val.Str fn.it) @> fn.at, es, ferr) @> at $sloc }
  | LBRACE; fe = expr_target; RBRACE; LPAREN; es = separated_list(COMMA, expr_target); RPAREN; ferr = catch_target?;
    { EExpr.Call (fe, es, ferr) @> at $sloc }
  | EXTERN; fn = id_target; LPAREN; es = separated_list(COMMA, expr_target); RPAREN;
    { EExpr.ECall (fn, es) @> at $sloc }
  | LBRACE; flds = separated_list(COMMA, field_init_target); RBRACE;
    { EExpr.NewObj (EExpr.Parser.parse_obj_flds flds) @> at $sloc }
  | oe = expr_target; fe = lookup_target;
    { EExpr.Lookup (oe, fe) @> at $sloc }
  | LBRACE; fe = expr_target; RBRACE; ATSIGN; LPAREN; es = separated_list(COMMA, expr_target); RPAREN;
    { EExpr.Curry (fe, es) @> at $sloc }

let nopt_target :=
  | LARRBRACK; es = separated_list (COMMA, expr_target); RARRBRACK;
    { EExpr.NOpt (ArrayExpr, es) @> at $sloc }
  | LBRACK; es = separated_list (COMMA, expr_target); RBRACK;
    { EExpr.NOpt (ListExpr, es) @> at $sloc }
  | LPAREN; v = expr_target; COMMA; vs = separated_nonempty_list(COMMA, expr_target); RPAREN;
    { EExpr.NOpt (TupleExpr, v :: vs) @> at $sloc }

let catch_target := CATCH; ~ = id_target; <>

let field_init_target :=
  | ~ = id_target; COLON; ~ = expr_target; <>
  | ~ = str_id_target; COLON; ~ = expr_target; <>

let lookup_target :=
  | PERIOD; fn = id_target;                       { EExpr.Val (Val.Str fn.it) @> at $sloc }
  | LBRACK; fe = expr_target; RBRACK;             { fe }

(* ==================== Values ==================== *)

let id_target := x = ID;          { (x @> at $sloc) }

let gid_target := x = GID;        { (x @> at $sloc) }

let str_id_target := s = STRING;  { (s @> at $sloc) }

let val_target :=
  | NULL;               { Val.Null }
  | i = INT;            < Val.Int >
  | f = FLOAT;          < Val.Flt >
  | s = STRING;         < Val.Str >
  | b = BOOLEAN;        < Val.Bool >
  | s = SYMBOL;         < Val.Symbol >
  | l = LOC;            < Val.Loc >
  | t = dtype_target;   < Val.Type >

let dtype_target :=
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

(* ==================== Operators ==================== *)

let const_target ==
  | MAX_VALUE;              { Operator.MAX_VALUE }
  | MIN_VALUE;              { Operator.MIN_VALUE }
  | PI;                     { Operator.PI }

let binopt_infix_target ==
  | ~ = core_binopt_infix;  <>
  | SCLAND;                 { Operator.SCLogicalAnd }
  | SCLOR;                  { Operator.SCLogicalOr }
  | OBJECT_MEM;             { Operator.ObjectMem }

let unopt_call_target ==
  | ~ = core_unopt_call;    <>
  | OBJECT_TO_LIST;         { Operator.ObjectToList }
  | OBJECT_FIELDS;          { Operator.ObjectFields }

let triopt_call_target ==
  | ~ = core_triopt;        <>
  | ITE;                    { Operator.ITE }

(* ==================== Metadata ==================== *)

let vals_metadata_target := ~ = separated_list(COMMA, val_target); <>

let var_metadata_target :=
  | meta = STRING;
    {
      let param_alt = String.split_on_char ':' meta in
      if List.length param_alt = 2 then ( List.nth param_alt 0, List.nth param_alt 1 )
      else raise (Failure "Invalid function's variables metadata")
    }

let stmt_metadata_target :=
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

let var_opt_metadata_target :=
  | meta = delimited(LBRACK, var_metadata_target, RBRACK)?;
    { meta }

let vars_opt_metadata_target :=
  | meta = delimited(LBRACK, separated_list(COMMA, var_metadata_target), RBRACK)?;
    { Option.value ~default:[] meta }

let stmt_opt_metadata_target :=
  | meta = delimited(LBRACK, stmt_metadata_target, RBRACK)?;
    { Option.value ~default:[] meta }

let str_opt_metadata_target :=
  | meta = delimited(LBRACK, STRING, RBRACK)?;
    { Option.value ~default:"" meta }

(* ==================== Type system ==================== *)

let tannot_target := COLON; t = type_target; <>

let type_target :=
  | ~ = simple_type_target; <>
  | ~ = nary_type_target; <>

let simple_type_target :=
  | LPAREN; t = type_target; RPAREN;
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
  | LBRACK; t = type_target; RBRACK;
    { EType.ListType t }
  | LBRACE; props = separated_list (COMMA, type_property_target); RBRACE;
    { EType.ObjectType (EType.parse_obj_type props) }
  | v = ID;
    { EType.UserDefinedType v }

let nary_type_target :=
  | t1 = simple_type_target; merge_func = nary_type_op_target; t2 = type_target;
    { merge_func t1 t2 }
  | STYPE_SIGMA; LBRACK; d = ID; RBRACK; PIPE?; t = nary_type_target;
    { EType.parse_sigma_type d t }

let nary_type_op_target :=
  | TIMES;        { EType.merge_tuple_type }
  | PIPE;         { EType.merge_union_type }

let type_property_target :=
  | v = ID; COLON; t = type_target;
    { EType.Field.NamedField (v, (t, false) ) }
  | v = ID; QUESTION; COLON; t = type_target;
    { EType.Field.NamedField (v, (t, true) ) }
  | TIMES; COLON; t = type_target;
    { EType.Field.SumryField t }
