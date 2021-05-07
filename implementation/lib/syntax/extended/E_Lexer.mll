(*
  The first section is
    an optional chunk of OCaml code that is bounded by a pair of curly braces.
  Define utility functions used by later snippets of OCaml code and
    set up the environment by opening useful modules and define exceptions.
*)
{
  open Lexing
  open E_Parser


  (* https://www.ocaml.org/releases/4.11/htmlman/lexyacc.html#s:lexyacc-common-errors *)
  let keyword_table = Hashtbl.create 53
    let _ =
      List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
              [
                "octal_to_decimal", OCTAL_TO_DECIMAL;
                "hex_decode"      , HEX_DECODE;
                "utf8_decode"     , UTF8_DECODE;
                "float_to_string" , FLOAT_TO_STRING;
                "float_of_string" , FLOAT_OF_STRING;
                "obj_to_list"     , OBJ_TO_LIST;
                "obj_fields"      , OBJ_FIELDS;
                "to_int"          , TO_INT;
                "to_int32"        , TO_INT32;
                "to_uint32"       , TO_UINT32;
                "to_uint16"       , TO_UINT16;
                "from_char_code"  , FROM_CHAR_CODE;
                "from_char_code_u", FROM_CHAR_CODE_U;
                "to_char_code"    , TO_CHAR_CODE;
                "to_char_code_u"  , TO_CHAR_CODE_U;
                "to_lower_case"   , TO_LOWER_CASE;
                "to_upper_case"   , TO_UPPER_CASE;
                "trim"            , TRIM;
                "abs"             , ABS;
                "acos"            , ACOS;
                "asin"            , ASIN;
                "atan"            , ATAN;
                "atan2"           , ATAN_2;
                "ceil"            , CEIL;
                "cos"             , COS;
                "exp"             , EXP;
                "floor"           , FLOOR;
                "log_e"           , LOG_E;
                "log_10"          , LOG_10;
                "max"             , MAX;
                "min"             , MIN;
                "random"          , RANDOM;
                "sin"             , SIN;
                "sqrt"            , SQRT;
                "tan"             , TAN;
                "PI"              , PI;
                "MAX_VALUE"       , MAX_VALUE;
                "MIN_VALUE"       , MIN_VALUE;
                "in_obj"          , IN_OBJ;
                "in_list"         , IN_LIST;
                "l_len"           , LLEN;
                "l_nth"           , LNTH;
                "l_add"           , LADD;
                "l_prepend"       , LPREPEND;
                "l_concat"        , LCONCAT;
                "l_remove_last"   , LREMOVELAST;
                "l_sort"          , LSORT;
                "hd"              , HD;
                "tl"              , TL;
                "t_len"           , TLEN;
                "t_nth"           , TNTH;
                "fst"             , FST;
                "snd"             , SND;
                "s_split"         , SSPLIT;
                "s_concat"        , SCONCAT;
                "s_len"           , SLEN;
                "s_len_u"         , SLEN_U;
                "s_nth"           , SNTH;
                "s_nth_u"         , SNTH_U;
                "s_substr"        , SSUBSTR;
                "s_substr_u"      , SSUBSTR_U;
                "int_to_float"    , INT_TO_FLOAT;
                "int_to_string"   , INT_TO_STRING;
                "int_of_string"   , INT_OF_STRING;
                "int_of_float"    , INT_OF_FLOAT;
                "int_to_four_hex" , INT_TO_FOUR_HEX;
                "typeof"          , TYPEOF;
                "catch"           , CATCH;
                "throw"           , THROW;
                "fail"            , FAIL;
                "import"          , IMPORT;
                "None"            , NONE;
                "default"         , DEFAULT;
                "if"              , IF;
                "else"            , ELSE;
                "elif"            , ELIF;
                "while"           , WHILE;
                "foreach"         , FOREACH;
                "return"          , RETURN;
                "function"        , FUNCTION;
                "macro"           , MACRO;
                "delete"          , DELETE;
                "null"            , NULL;
                "repeat"          , REPEAT;
                "until"           , UNTIL;
                "match"           , MATCH;
                "with"            , WITH;
                "print"           , PRINT;
                "gen_wrapper"     , WRAPPER;
                "assert"          , ASSERT;
                "switch"          , SWITCH;
                "case"            , CASE;
                "sdefault"        , SDEFAULT;
                "NaN"             , FLOAT (float_of_string "nan");
                "Infinity"        , FLOAT (float_of_string "infinity");
                "lambda"          , LAMBDA;
                "extern"          , EXTERN
                ]

  exception Syntax_error of string


  let create_syntax_error ?(eof=false) (msg : string) (lexbuf : Lexing.lexbuf) : exn =
    let c = Lexing.lexeme lexbuf in
    let formatted_msg = (
      match eof with
      | true  -> Printf.sprintf "%s. Line number: %d. File: %s" msg (lexbuf.lex_curr_p.pos_lnum) (lexbuf.lex_curr_p.pos_fname)
      | false -> Printf.sprintf "%s: %s. Line number: %d. File: %s" msg c (lexbuf.lex_curr_p.pos_lnum) (lexbuf.lex_curr_p.pos_fname)
    ) in (Syntax_error formatted_msg)
}

(*
  The second section is
    a collection of named regular expressions.
*)
let digit   = ['0' - '9']
let letter  = ['a' - 'z' 'A' - 'Z']
let int     = '-'?digit+
let frac    = '.' digit*
let exp     = ['e' 'E'] ['-' '+']? digit+
let float   = digit* frac? exp?
let bool    = "true"|"false"
let var     = (letter | '_'*letter)(letter|digit|'_'|'\'')*
let gvar    = '|'(var)'|'
let symbol  = '\''(var|int)
let white   = (' '|'\t')+
let newline = '\r'|'\n'|"\r\n"
let loc     = "$loc_"(digit|letter|'_')+
let hex_digit = (digit | ['a'-'f' 'A'-'F'])

(*
  The third section is
    the one with the lexing rules: functions that consume the data,
    producing OCaml expressions that evaluate to tokens.
  The rules are structured very similarly to pattern matches,
    except that the variants are replaced by regular expressions on the left-hand side.
    The righthand-side clause is the parsed OCaml return value of that rule.
    The OCaml code for the rules has a parameter called lexbuf that defines the input,
    including the position in the input file, as well as the text that was matched
    by the regular expression.
  "Lexing.lexeme lexbuf" returns the complete string matched by the regular expression.
*)
rule read =
  parse

  | white          { read lexbuf }
  | newline        { new_line lexbuf; read lexbuf }
  | ":="           { DEFEQ }
  | '@'            { AT_SIGN }
  | '.'            { PERIOD }
  | ';'            { SEMICOLON }
  | ':'            { COLON }
  | ','            { COMMA }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIVIDE }
  | '%'            { MODULO }
  | '='            { EQUAL }
  | '>'            { GT }
  | '<'            { LT }
  | ">="           { EGT }
  | "<="           { ELT }
  | '!'            { NOT }
  | '~'            { BITWISE_NOT }
  | '&'            { BITWISE_AND }
  | '|'            { PIPE }
  | '^'            { BITWISE_XOR }
  | "<<"           { SHIFT_LEFT }
  | ">>"           { SHIFT_RIGHT }
  | ">>>"          { SHIFT_RIGHT_LOGICAL }
  | "&&&"          { SCLAND }
  | "|||"          { SCLOR }
  | "&&"           { LAND }
  | "||"           { LOR }
  | "**"           { POW }
  | "->"           { RIGHT_ARROW }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LBRACE }
  | '}'            { RBRACE }
  | '['            { LBRACK }
  | ']'            { RBRACK }
  | "__$"          { read_type lexbuf }
  | int            { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float          { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | bool           { BOOLEAN (bool_of_string (Lexing.lexeme lexbuf)) }
  | '"'            { read_string (Buffer.create 16) lexbuf }
  | gvar           { GVAR (String_Utils.trim_ends (Lexing.lexeme lexbuf))}
  | letter(letter|digit|'_')* as id { try
                                        Hashtbl.find keyword_table id
                                      with Not_found -> VAR id }
  | var            { VAR (Lexing.lexeme lexbuf) }
  | symbol         { SYMBOL (String_Utils.chop_first_char (Lexing.lexeme lexbuf)) }
  | loc            { LOC (Lexing.lexeme lexbuf) }
  | "/*"           { read_comment lexbuf }
  | _              { raise (create_syntax_error "Unexpected char" lexbuf) }
  | eof            { EOF }


(* Read strings *)
and read_string buf =
  parse
  | '"'                  { STRING (Buffer.contents buf) }
  | '\\' '/'             { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'            { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'             { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'v'             { Buffer.add_char buf '\011'; read_string buf lexbuf }
  | '\\' 'f'             { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'             { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'             { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'             { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '\"'            { Buffer.add_char buf '\"'; read_string buf lexbuf }
  | '\\' '\''            { Buffer.add_char buf '\''; read_string buf lexbuf }
  | '\\' '0'             { Buffer.add_char buf '\000'; read_string buf lexbuf }
  | '\\' 'x' hex_digit as h1 hex_digit as h2
                         {
                           Buffer.add_string buf (String_Utils.hexdecode ("\\x" ^ h1 ^ h2));
                           read_string buf lexbuf
                         }
  | '\\' 'u' hex_digit as h1 hex_digit as h2 hex_digit as h3 hex_digit as h4
                         {
                           Buffer.add_string buf (String_Utils.utf8decode ("\\u" ^ h1 ^ h2 ^ h3 ^ h4));
                           read_string buf lexbuf
                         }
  | [^ '"' '\\']+        {
                           Buffer.add_string buf (Lexing.lexeme lexbuf);
                           read_string buf lexbuf
                         }
  | _                    { raise (create_syntax_error "Illegal string character" lexbuf) }
  | eof                  { raise (create_syntax_error ~eof:true "String is not terminated" lexbuf) }


and read_comment =
(* Read comments *)
  parse
  | "*/"      { read lexbuf }
  | newline   { new_line lexbuf; read_comment lexbuf }
  | _         { read_comment lexbuf }
  | eof       { raise (create_syntax_error ~eof:true "Comment is not terminated" lexbuf)}

and read_type =
(* Read Language Types *)
  parse
  | "Int"    { INT_TYPE }
  | "Flt"    { FLT_TYPE }
  | "Bool"   { BOOL_TYPE }
  | "Str"    { STR_TYPE }
  | "Obj"    { LOC_TYPE }
  | "List"   { LIST_TYPE }
  | "Tuple"  { TUPLE_TYPE }
  | "Null"   { NULL_TYPE }
  | "Symbol" { SYMBOL_TYPE }
  | "Curry"  { CURRY_TYPE }
  | _        { raise (create_syntax_error "Unexpected type" lexbuf) }
