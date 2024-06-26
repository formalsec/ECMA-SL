(* ========== OCaml Utilities ========== *)

{
  open Lexing
  open EParser

  let keywords = Hashtbl.of_seq @@ List.to_seq
          [
            (* Language values *)
            "null"                    , NULL;
            "None"                    , NONE;

            (* Language constructs *)
            "import"                  , IMPORT;
            "macro"                   , MACRO;
            "print"                   , PRINT;
            "delete"                  , DELETE;
            "function"                , FUNCTION;
            "return"                  , RETURN;
            "extern"                  , EXTERN;
            "lambda"                  , LAMBDA;
            "if"                      , IF;
            "else"                    , ELSE;
            "elif"                    , ELIF;
            "while"                   , WHILE;
            "foreach"                 , FOREACH;
            "repeat"                  , REPEAT;
            "until"                   , UNTIL;
            "switch"                  , SWITCH;
            "case"                    , CASE;
            "sdefault"                , SDEFAULT;
            "match"                   , MATCH;
            "with"                    , WITH;
            "default"                 , DEFAULT;
            "throw"                   , THROW;
            "catch"                   , CATCH;
            "fail"                    , FAIL;
            "assert"                  , ASSERT;
            "gen_wrapper"             , WRAPPER;

            (* Program constants *)
            "NaN"                     , FLOAT (Float.nan);
            "Infinity"                , FLOAT (Float.infinity);
            "MAX_VALUE"               , MAX_VALUE;
            "MIN_VALUE"               , MIN_VALUE;
            "PI"                      , PI;

            (* Integer operators *)
            "int_to_float"            , INT_TO_FLOAT;
            "int_to_string"           , INT_TO_STRING;

            (* Float operators *)
            "int_of_float"            , FLOAT_TO_INT;
            "float_to_string"         , FLOAT_TO_STRING;

            (* String operators *)
            "int_of_string"           , STRING_TO_INT;
            "float_of_string"         , STRING_TO_FLOAT;
            "from_char_code"          , FROM_CHAR_CODE;
            "to_char_code"            , TO_CHAR_CODE;
            "s_len"                   , STRING_LEN;
            "s_concat"                , STRING_CONCAT;
            "s_nth"                   , STRING_NTH;
            "s_substr"                , STRING_SUBSTR;

            (* Object operators *)
            "obj_to_list"             , OBJECT_TO_LIST;
            "obj_fields"              , OBJECT_FIELDS;
            "in_obj"                  , OBJECT_MEM;

            (* List operators *)
            "hd"                      , LIST_HEAD;
            "tl"                      , LIST_TAIL;
            "l_len"                   , LIST_LEN;
            "l_reverse"               , LIST_REVERSE;
            "l_nth"                   , LIST_NTH;
            "l_add"                   , LIST_ADD;
            "l_prepend"               , LIST_PREPEND;
            "l_concat"                , LIST_CONCAT;
            "l_set"                   , LIST_SET;

            (* Math operators *)
            "abs"                     , ABS;
            "sqrt"                    , SQRT;
            "ceil"                    , CEIL;
            "floor"                   , FLOOR;
            "trunc"                   , TRUNC;

            (* Type system *)
            "typedef"                 , TYPEDEF;
            "any"                     , TYPE_ANY;
            "unknown"                 , TYPE_UNKNOWN;
            "never"                   , TYPE_NEVER;
            "undefined"               , TYPE_UNDEFINED;
            "void"                    , TYPE_VOID;
            "int"                     , TYPE_INT;
            "float"                   , TYPE_FLOAT;
            "string"                  , TYPE_STRING;
            "boolean"                 , TYPE_BOOLEAN;
            "symbol"                  , TYPE_SYMBOL;
            "sigma"                   , TYPE_SIGMA;
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



(* ========== Regular expressions ========== *)

let digit         = ['0' - '9']
let letter        = ['a' - 'z' 'A' - 'Z']
let int           = '-'?digit+
let frac          = '.' digit*
let exp           = ['e' 'E'] ['-' '+']? digit+
let float         = digit* frac? exp?
let bool          = "true" | "false"
let id            = (letter | '_'* letter) (letter | digit | '_' | '\'')*
let gid           = '|' (id) '|'
let symbol        = '\'' (id | int)
let white         = (' ' | '\t')+
let newline       = '\r' | '\n' | "\r\n"
let loc           = "$loc_" digit+
let hex_digit     = (digit | ['a' - 'f' 'A' - 'F'])
let hex_literal   = "0x" hex_digit hex_digit? hex_digit? hex_digit? hex_digit? hex_digit?



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
  | '?'               { QUESTION }
  | '!'               { EXCLAMATION }
  | "&&"              { LAND }
  | "||"		          { LOR }
  | "|||"             { SCLOR }
  | "&&&"             { SCLAND }
  | '='               { EQ }
  | "!="              { NEQ }
  | '<'               { LT }
  | '>'               { GT }
  | "<="              { LE }
  | ">="              { GE }
  | "->"              { RIGHT_ARROW }
  | '"'               { read_string (Buffer.create 16) lexbuf }
  | int               { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float             { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | bool              { BOOLEAN (bool_of_string (Lexing.lexeme lexbuf)) }
  | hex_literal       { INT(Stdlib.int_of_string (Lexing.lexeme lexbuf)) }
  | id as x           { try Hashtbl.find keywords x with Not_found -> ID x }
  | gid               { GID (String_utils.trim_ends (Lexing.lexeme lexbuf))}
  | symbol            { SYMBOL (String_utils.chop_first_char (Lexing.lexeme lexbuf)) }
  | loc               { LOC (Parsing_utils.parse_loc @@ Lexing.lexeme lexbuf) }
  | "/*"              { read_comment lexbuf }
  | _                 { raise (create_syntax_error "Unexpected char" lexbuf) }
  | eof               { EOF }



(* ========== String reader ========== *)

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

(* ========== Comment reader ========== *)

and read_comment =
  parse
  | "*/"    { read lexbuf }
  | newline { new_line lexbuf; read_comment lexbuf }
  | _       { read_comment lexbuf }
  | eof     { raise (create_syntax_error ~eof:true "Comment is not terminated" lexbuf)}
