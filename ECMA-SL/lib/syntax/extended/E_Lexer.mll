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
                (* Language *)
                "if"                    , IF;
                "else"                  , ELSE;
                "elif"                  , ELIF;
                "while"                 , WHILE;
                "foreach"               , FOREACH;
                "repeat"                , REPEAT;
                "until"                 , UNTIL;
                "switch"                , SWITCH;
                "case"                  , CASE;
                "sdefault"              , SDEFAULT;
                "match"                 , MATCH;
                "with"                  , WITH;
                "default"               , DEFAULT;
                "None"                  , NONE;
                "null"                  , NULL;
                "function"              , FUNCTION;
                "return"                , RETURN;
                "lambda"                , LAMBDA;
                "macro"                 , MACRO;
                "catch"                 , CATCH;
                "throw"                 , THROW;
                "fail"                  , FAIL;
                "assert"                , ASSERT;
                "import"                , IMPORT;
                "extern"                , EXTERN;
                "print"                 , PRINT;
                "delete"                , DELETE;
                "typeof"                , TYPEOF;
                "gen_wrapper"           , WRAPPER;
                
                (* Constants *)
                "NaN"                   , FLOAT (float_of_string "nan");
                "Infinity"              , FLOAT (float_of_string "infinity");
                "PI"                    , PI;
                "MAX_VALUE"             , MAX_VALUE;
                "MIN_VALUE"             , MIN_VALUE;

                (* Built-Ins *)
                "is_NaN"                , IS_NAN;
                "to_int"                , TO_INT;
                "to_int32"              , TO_INT32;
                "to_uint16"             , TO_UINT16;
                "to_uint32"             , TO_UINT32;
                "int_to_float"          , INT_TO_FLOAT;
                "int_of_float"          , INT_OF_FLOAT;
                "int_to_string"         , INT_TO_STRING;
                "int_of_string"         , INT_OF_STRING;
                "int_to_four_hex"       , INT_TO_FOUR_HEX;
                "int_to_be_bytes"       , INT_TO_BE_BYTES;
                "int_from_le_bytes"     , INT_FROM_BYTES;
                "octal_to_decimal"      , OCTAL_TO_DECIMAL;
                "uint_from_le_bytes"    , UINT_FROM_BYTES;

                "float_to_string"       , FLOAT_TO_STRING;
                "float_of_string"       , FLOAT_OF_STRING;
                "float_to_byte"         , FLOAT_TO_BYTE;
                "float64_to_le_bytes"   , FLOAT64_TO_LE_BYTES; 
                "float64_to_be_bytes"   , FLOAT64_TO_BE_BYTES; 
                "float32_to_le_bytes"   , FLOAT32_TO_LE_BYTES; 
                "float32_to_be_bytes"   , FLOAT32_TO_BE_BYTES; 
                "float64_from_le_bytes" , FLOAT64_FROM_LE_BYTES; 
                "float64_from_be_bytes" , FLOAT64_FROM_BE_BYTES; 
                "float32_from_le_bytes" , FLOAT32_FROM_LE_BYTES; 
                "float32_from_be_bytes" , FLOAT32_FROM_BE_BYTES;

                "bytes_to_string"       , BYTES_TO_STRING;
                "utf8_decode"           , UTF8_DECODE;
                "hex_decode"            , HEX_DECODE;
                "from_char_code"        , FROM_CHAR_CODE;
                "from_char_code_u"      , FROM_CHAR_CODE_U;
                "to_char_code"          , TO_CHAR_CODE;
                "to_char_code_u"        , TO_CHAR_CODE_U;
                "to_lower_case"         , TO_LOWER_CASE;
                "to_upper_case"         , TO_UPPER_CASE;
                "trim"                  , TRIM;

                "random"                , RANDOM;
                "abs"                   , ABS;
                "sqrt"                  , SQRT;
                "ceil"                  , CEIL;
                "floor"                 , FLOOR;
                "PI"                    , PI;
                "MAX_VALUE"             , MAX_VALUE;
                "MIN_VALUE"             , MIN_VALUE;
                "exp"                   , EXP;
                "log_2"                 , LOG_2;
                "log_e"                 , LOG_E;
                "log_10"                , LOG_10;
                "cos"                   , COS;
                "sin"                   , SIN;
                "tan"                   , TAN;
                "cosh"                  , COSH;
                "sinh"                  , SINH;
                "tanh"                  , TANH;
                "acos"                  , ACOS;
                "asin"                  , ASIN;
                "atan"                  , ATAN;
                "atan2"                 , ATAN_2;
                "max"                   , MAX;
                "min"                   , MIN;
                "to_precision"          , TO_PRECISION;
                "to_exponential"        , TO_EXPONENTIAL;
                "to_fixed"              , TO_FIXED;

                "parse_number"          , PARSE_NUMBER;
                "parse_string"          , PARSE_STRING;
                "parse_date"            , PARSE_DATE;

                "s_len"                 , SLEN;
                "s_len_u"               , SLEN_U;
                "s_concat"              , SCONCAT;
                "s_split"               , SSPLIT;
                "s_nth"                 , SNTH;
                "s_nth_u"               , SNTH_U;
                "s_substr"              , SSUBSTR;
                "s_substr_u"            , SSUBSTR_U;

                "a_nth"                 , ANTH;
                "a_set"                 , ASET;
                "l_set"                 , LSET;
                "a_len"                 , ALEN;
                "array_make"            , ARRAY_MAKE;
                "list_to_array"         , LIST_TO_ARRAY;

                "hd"                    , HD;
                "tl"                    , TL;
                "l_len"                 , LLEN;
                "l_nth"                 , LNTH;
                "l_add"                 , LADD;
                "l_prepend"             , LPREPEND;
                "l_concat"              , LCONCAT;
                "l_remove"              , LREM;
                "l_remove_nth"          , LREMNTH;
                "l_remove_last"         , LREMOVELAST;
                "l_reverse"             , LREVERSE;
                "l_sort"                , LSORT;
                "in_list"               , IN_LIST;

                "fst"                   , FST;
                "snd"                   , SND;
                "t_len"                 , TLEN;
                "t_nth"                 , TNTH;

                "obj_to_list"           , OBJ_TO_LIST;
                "obj_fields"            , OBJ_FIELDS;
                "in_obj"                , IN_OBJ;

                (* API *)
                "se_assume"             , API_ASSUME;
                "se_abort"              , API_ABORT;
                "se_is_sat"             , API_IS_SAT;
                "se_is_symbolic"        , API_IS_SYMBOLIC;
                "se_is_number"          , API_IS_NUMBER;
                "se_mk_symbolic"        , API_MK_SYMBOLIC;
                "se_evaluate"           , API_EVAL;
                "se_maximize"           , API_MAXIMIZE;
                "se_minimize"           , API_MINIMIZE;

                (* Types *)
                "typedef"               , TYPEDEF;
                "any"                   , TYPE_ANY;
                "unknown"               , TYPE_UNKNOWN;
                "never"                 , TYPE_NEVER;
                "undefined"             , TYPE_UNDEFINED;
                "void"                  , TYPE_VOID;
                "int"                   , TYPE_INT;
                "float"                 , TYPE_FLOAT;
                "string"                , TYPE_STRING;
                "boolean"               , TYPE_BOOLEAN;
                "symbol"                , TYPE_SYMBOL;
                "sigma"                 , TYPE_SIGMA;
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
let var     = (letter | '_')(letter|digit|'_'|'\'')*
let gvar    = '|'(var)'|'
let symbol  = '\''(var|int)
let white   = (' '|'\t')+
let newline = '\r'|'\n'|"\r\n"
let loc     = "$loc_"(digit|letter|'_')+
let hex_digit = (digit | ['a' - 'f' 'A' - 'F'])
let hex_literal = "0x" hex_digit hex_digit? hex_digit? hex_digit? hex_digit? hex_digit?

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
  (*| "[|"           { LARRBRACK }
  | "|]"           { RARRBRACK }*)
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
  | '?'            { QUESTION }
  | "__$"          { read_type lexbuf }
  | int            { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float          { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | bool           { BOOLEAN (bool_of_string (Lexing.lexeme lexbuf)) }
  | '"'            { read_string (Buffer.create 16) lexbuf }
  | gvar           { GVAR (String_utils.trim_ends (Lexing.lexeme lexbuf))}
  | var as x       { try Hashtbl.find keyword_table x with Not_found -> VAR x }
  | symbol         { SYMBOL (String_utils.chop_first_char (Lexing.lexeme lexbuf)) }
  | loc            { LOC (Lexing.lexeme lexbuf) }
  | hex_literal    { INT(Stdlib.int_of_string (Lexing.lexeme lexbuf)) }
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
  | '\\' 'x' hex_digit hex_digit as h
                         {
                           Buffer.add_string buf (String_utils.hexdecode h);
                           read_string buf lexbuf
                         }
  | '\\' 'u' '{' hex_digit hex_digit hex_digit hex_digit hex_digit? hex_digit? '}' as h
                         {
                           Buffer.add_string buf (String_utils.utf8decode h);
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
