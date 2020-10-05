(*
  The first section is
    an optional chunk of OCaml code that is bounded by a pair of curly braces.
  Define utility functions used by later snippets of OCaml code and
    set up the environment by opening useful modules and define exceptions.
*)
{
  open Lexing
  open E_Parser



  exception Syntax_error of string
}

(*
  The second section is
    a collection of named regular expressions.
*)
let digit   = ['0' - '9']
let letter  = ['a' - 'z' 'A' - 'Z']
let special = ('_'|' '|','|';'|'.'|':'|'\\' '"'|'/'|'*'|'-'|'+'|'<'|'>'|'='|'{'|'}'|'['|']'|'('|')'|'$'|'@'|'!'|'?')
let int     = '-'?digit+
let float   = int('.')digit*
let bool    = "true"|"false"
let string  = '"'(digit|letter|special)*'"'
let var     = (letter | '_'*letter)(letter|digit|'_'|'\'')*
let symbol  = '\''('+'|'-')*(var|int)
let white   = (' '|'\t')+
let newline = '\r'|'\n'|"\r\n"
let loc     = "$loc_"(digit|letter|'_')+

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
  | newline        { read lexbuf }
  | ":="           { DEFEQ }
  | '.'            { PERIOD }
  | ';'            { SEMICOLON }
  | ':'            { COLON }
  | ','            { COMMA }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIVIDE }
  | '='            { EQUAL }
  | '>'            { GT }
  | '<'            { LT }
  | ">="           { EGT }
  | "<="           { ELT }
  | "in_obj"       { IN_OBJ }
  | "in_list"      { IN_LIST }
  | '!'            { NOT }
  | "&&"           { LAND }
  | "||"           { LOR }
  | "l_len"        { LLEN }
  | "l_nth"        { LNTH }
  | "l_add"        { LADD }
  | "l_prepend"    { LPREPEND }
  | "l_concat"     { LCONCAT }
  | "hd"           { HD }
  | "tl"           { TL }
  | "t_len"        { TLEN }
  | "t_nth"        { TNTH }
  | "fst"          { FST }
  | "snd"          { SND }
  | "s_concat"     { SCONCAT }
  | "int_to_float"    { INT_TO_FLOAT }
  | "int_to_string"   { INT_TO_STRING }
  | "float_to_string" { FLOAT_TO_STRING }
  | "obj_to_list"  { OBJ_TO_LIST }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LBRACE }
  | '}'            { RBRACE }
  | '['            { LBRACK }
  | ']'            { RBRACK }
  | '|'            { PIPE }
  | "typeof"       { TYPEOF }
  | "__$"          { read_type lexbuf }
  | "throw"        { THROW }
  | "import"       { IMPORT }
  | "->"           { RIGHT_ARROW }
  | "None"         { NONE }
  | "default"      { DEFAULT }
  | "if"           { IF }
  | "else"         { ELSE }
  | "while"        { WHILE }
  | "return"       { RETURN }
  | "function"     { FUNCTION }
  | "delete"       { DELETE }
  | "null"         { NULL }
  | "repeat"       { REPEAT }
  | "until"        { UNTIL }
  | "match"        { MATCH }
  | "with"         { WITH }
  | "print"        { PRINT }
  | int            { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float          { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | bool           { BOOLEAN (bool_of_string (Lexing.lexeme lexbuf)) }
  | string         { STRING (Lexing.lexeme lexbuf) }
  | var            { VAR (Lexing.lexeme lexbuf) }
  | symbol         { SYMBOL (Lexing.lexeme lexbuf) }
  | loc            { LOC (Lexing.lexeme lexbuf) }
  | "/*"           { read_comment lexbuf }
  | _              { raise (Syntax_error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof            { EOF }

and read_comment =
(* Read comments *)
  parse
  | "*/" { read lexbuf }
  | _    { read_comment lexbuf }
  | eof  { raise (Syntax_error ("Comment is not terminated."))}

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
  | _        { raise (Syntax_error ("Unexpected type: " ^ Lexing.lexeme lexbuf)) }
