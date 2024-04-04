(* ========== OCaml Utilities ========== *)

{
  open Lexing
  open Parser

  let keywords = Hashtbl.of_seq @@ List.to_seq
          [
            (* Language values *)
            "null"                    , NULL;

            (* Language constructs *)
            "print"                   , PRINT;
            "delete"                  , DELETE;
            "function"                , FUNCTION;
            "return"                  , RETURN;
            "extern"                  , EXTERN;
            "if"                      , IF;
            "else"                    , ELSE;
            "while"                   , WHILE;
            "switch"                  , SWITCH;
            "case"                    , CASE;
            "sdefault"                , SDEFAULT;
            "fail"                    , FAIL;
            "assert"                  , ASSERT;

            (* General operators *)
            "typeof"                  , TYPEOF;

            (* Integer operators *)
            "int_to_float"            , INT_TO_FLOAT;
            "int_to_string"           , INT_TO_STRING;

            (* Float operators *)
            "int_of_float"            , FLOAT_TO_INT;
            "float_to_string"         , FLOAT_TO_STRING;
            "to_int"                  , TO_INT;
            "to_int32"                , TO_INT32;
            "to_uint16"               , TO_UINT16;
            "to_uint32"               , TO_UINT32;
            "is_NaN"                  , IS_NAN;
            "to_precision"            , TO_PRECISION;
            "to_exponential"          , TO_EXPONENTIAL;
            "to_fixed"                , TO_FIXED;

            (* String operators *)
            "int_of_string"           , STRING_TO_INT;
            "float_of_string"         , STRING_TO_FLOAT;
            "from_char_code"          , FROM_CHAR_CODE;
            "from_char_code_u"        , FROM_CHAR_CODE_U;
            "to_char_code"            , TO_CHAR_CODE;
            "to_char_code_u"          , TO_CHAR_CODE_U;
            "to_lower_case"           , TO_LOWER_CASE;
            "to_upper_case"           , TO_UPPER_CASE;
            "trim"                    , TRIM;
            "s_len"                   , STRING_LEN;
            "s_len_u"                 , STRING_LEN_U;
            "s_concat"                , STRING_CONCAT;
            "s_nth"                   , STRING_NTH;
            "s_nth_u"                 , STRING_NTH_U;
            "s_split"                 , STRING_SPLIT;
            "s_substr"                , STRING_SUBSTR;
            "s_substr_u"              , STRING_SUBSTR_U;

            (* Object operators *)
            "obj_to_list"             , OBJECT_TO_LIST;
            "obj_fields"              , OBJECT_FIELDS;
            "in_obj"                  , OBJECT_MEM;

            (* Array operators *)
            "a_len"                   , ARRAY_LEN;
            "array_make"              , ARRAY_MAKE;
            "a_nth"                   , ARRAY_NTH;
            "a_set"                   , ARRAY_SET;

            (* List operators *)
            "list_to_array"           , LIST_TO_ARRAY;
            "hd"                      , LIST_HEAD;
            "tl"                      , LIST_TAIL;
            "l_len"                   , LIST_LEN;
            "l_sort"                  , LIST_SORT;
            "l_reverse"               , LIST_REVERSE;
            "l_remove_last"           , LIST_REMOVE_LAST;
            "in_list"                 , LIST_MEM;
            "l_nth"                   , LIST_NTH;
            "l_add"                   , LIST_ADD;
            "l_prepend"               , LIST_PREPEND;
            "l_concat"                , LIST_CONCAT;
            "l_remove"                , LIST_REMOVE;
            "l_remove_nth"            , LIST_REMOVE_NTH;
            "l_set"                   , LIST_SET;

            (* Tuple operators *)
            "fst"                     , TUPLE_FIRST;
            "snd"                     , TUPLE_SECOND;
            "t_len"                   , TUPLE_LEN;
            "t_nth"                   , TUPLE_NTH;

            (* Byte operators *)
            "float_to_byte"           , FLOAT_TO_BYTE;
            "float32_to_le_bytes"     , FLOAT32_TO_LE_BYTES;
            "float32_to_be_bytes"     , FLOAT32_TO_BE_BYTES;
            "float64_to_le_bytes"     , FLOAT64_TO_LE_BYTES;
            "float64_to_be_bytes"     , FLOAT64_TO_BE_BYTES;
            "float32_from_le_bytes"   , FLOAT32_FROM_LE_BYTES;
            "float32_from_be_bytes"   , FLOAT32_FROM_BE_BYTES;
            "float64_from_le_bytes"   , FLOAT64_FROM_LE_BYTES;
            "float64_from_be_bytes"   , FLOAT64_FROM_BE_BYTES;
            "bytes_to_string"         , BYTES_TO_STRING;
            "int_to_be_bytes"         , INT_TO_BE_BYTES;
            "int_from_le_bytes"       , INT_FROM_LE_BYTES;
            "uint_from_le_bytes"      , UINT_FROM_LE_BYTES;

            (* Math operators *)
            "random"                  , RANDOM;
            "abs"                     , ABS;
            "sqrt"                    , SQRT;
            "ceil"                    , CEIL;
            "floor"                   , FLOOR;
            "exp"                     , EXP;
            "log_2"                   , LOG_2;
            "log_e"                   , LOG_E;
            "log_10"                  , LOG_10;
            "sin"                     , SIN;
            "cos"                     , COS;
            "tan"                     , TAN;
            "sinh"                    , SINH;
            "cosh"                    , COSH;
            "tanh"                    , TANH;
            "asin"                    , ASIN;
            "acos"                    , ACOS;
            "atan"                    , ATAN;
            "min"                     , MIN;
            "max"                     , MAX;
            "atan2"                   , ATAN_2;

            (* Parse operators *)
            "utf8_decode"             , UTF8_DECODE;
            "hex_decode"              , HEX_DECODE;
            "parse_number"            , PARSE_NUMBER;
            "parse_string"            , PARSE_STRING;
            "parse_date"              , PARSE_DATE;
          ]

  exception Syntax_error of string

  let create_syntax_error ?(eof=false) (msg : string) (lexbuf : Lexing.lexbuf) : exn =
    let c = Lexing.lexeme lexbuf in
    let formatted_msg = (
      match eof with
      | true  -> Printf.sprintf "%s. Line number: %d." msg (lexbuf.lex_curr_p.pos_lnum)
      | false -> Printf.sprintf "%s: %s. Line number: %d." msg c (lexbuf.lex_curr_p.pos_lnum)
    ) in (Syntax_error formatted_msg)
}



(* ========== Regular expressions ========== *)

let digit         = ['0' - '9']
let letter        = ['a' - 'z' 'A' - 'Z']
let int           = '-'?digit+
let frac          = '.' digit*
let exp           = ['e' 'E'] ['-' '+']? digit+
let float         = digit* frac? exp? | "nan" | "inf"
let bool          = "true" | "false"
let id            = (letter | '_'* letter) (letter | digit | '_' | '\'')*
let symbol        = '\'' (id | int)
let white         = (' ' | '\t')+
let newline       = '\r' | '\n' | "\r\n"
let loc           = "$loc_" digit+
let three_d       = digit digit digit
let char_code     = '\\' three_d



(* ========== Lexical rules ========== *)

rule read =
  parse
  | white             { read lexbuf }
  | newline           { new_line lexbuf; read lexbuf }
  | '.'               { PERIOD }
  | ','               { COMMA }
  | ';'               { SEMICOLON }
  | ':'               { COLON }
  | ":="              { DEFEQ }
  | "@"               { ATSIGN }
  | "#"               { HASH }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | '{'               { LBRACE }
  | '}'               { RBRACE }
  | '['               { LBRACK }
  | ']'               { RBRACK }
  | "[|"              { LARRBRACK }
  | "|]"              { RARRBRACK }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { TIMES }
  | '/'               { DIVIDE }
  | '%'               { MODULO }
  | "**"              { POW }
  | '~'               { TILDE }
  | '&'               { AMPERSAND }
  | '|'               { PIPE }
  | '^'               { CARET }
  | "<<"              { SHIFT_LEFT }
  | ">>"              { SHIFT_RIGHT }
  | ">>>"             { SHIFT_RIGHT_LOGICAL }
  | '!'               { EXCLAMATION }
  | "&&"              { LAND }
  | "||"		          { LOR }
  | '='               { EQ }
  | "!="              { NEQ }
  | '<'               { LT }
  | '>'               { GT }
  | "<="              { LE }
  | ">="              { GE }
  | "__$"             { read_type lexbuf }
  | '"'               { read_string (Buffer.create 16) lexbuf }
  | int               { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float             { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | bool              { BOOLEAN (bool_of_string (Lexing.lexeme lexbuf)) }
  | id as x           { try Hashtbl.find keywords x with Not_found -> ID x }
  | symbol            { SYMBOL (String_utils.chop_first_char (Lexing.lexeme lexbuf)) }
  | loc               { LOC (Parsing_utils.parse_loc @@ Lexing.lexeme lexbuf) }
  | "/*"              { read_comment lexbuf }
  | _                 { raise (create_syntax_error "Unexpected char" lexbuf) }
  | eof               { EOF }



(* ========== String reader ========== *)

and read_string buf =
  parse
  | '"'                   { STRING (Buffer.contents buf) }
  | '\\' '/'              { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'             { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'              { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'n'              { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'              { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'              { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '\"'             { Buffer.add_char buf '\"'; read_string buf lexbuf }
  | '\\' '\''             { Buffer.add_char buf '\''; read_string buf lexbuf }
  | '\\' '0'              { Buffer.add_char buf '\000'; read_string buf lexbuf }
  | '\\' (three_d as c)   {
                            Buffer.add_char buf (Char.chr (int_of_string c));
                            read_string buf lexbuf
                          }
  | char_code char_code   {
                            let s = Lexing.lexeme lexbuf in
                            let s' = "\"" ^ s ^ "\"" in
                            let s'' = Scanf.sscanf s' "%S" (fun s -> s) in
                            Buffer.add_string buf s'';
                            read_string buf lexbuf
                          }
  | [^ '"' '\\']+         {
                            Buffer.add_string buf (Lexing.lexeme lexbuf);
                            read_string buf lexbuf
                          }
  | _                     { raise (create_syntax_error "Illegal string character" lexbuf) }
  | eof                   { raise (create_syntax_error ~eof:true "String is not terminated" lexbuf) }

(* ========== Comment reader ========== *)

and read_comment =
  parse
  | "*/"    { read lexbuf }
  | newline { new_line lexbuf; read_comment lexbuf }
  | _       { read_comment lexbuf }
  | eof     { raise (create_syntax_error ~eof:true "Comment is not terminated" lexbuf)}

(* ========== Runtime type reader ========== *)

and read_type =
  parse
  | "Null"   { DTYPE_NULL }
  | "Int"    { DTYPE_INT }
  | "Flt"    { DTYPE_FLT }
  | "Str"    { DTYPE_STR }
  | "Bool"   { DTYPE_BOOL }
  | "Symbol" { DTYPE_SYMBOL }
  | "Obj"    { DTYPE_LOC }
  | "List"   { DTYPE_LIST }
  | "Tuple"  { DTYPE_TUPLE }
  | "Curry"  { DTYPE_CURRY }
  | _        { raise (create_syntax_error "Unexpected runtime type" lexbuf) }
