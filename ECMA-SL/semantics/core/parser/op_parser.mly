%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MODULO
%token POW
%token AMPERSAND
%token PIPE
%token CARET
%token SHIFT_LEFT SHIFT_RIGHT SHIFT_RIGHT_LOGICAL
%token LAND LOR
%token EQ NE LT GT LE GE

%token INT_TO_FLOAT INT_TO_STRING
%token FLOAT_TO_INT FLOAT_TO_STRING

%token STRING_TO_INT STRING_TO_FLOAT FROM_CHAR_CODE TO_CHAR_CODE
%token STRING_LEN STRING_CONCAT

%token STRING_NTH
%token STRING_SUBSTR

%token LIST_HEAD LIST_TAIL LIST_LEN LIST_REVERSE
%token LIST_NTH LIST_ADD LIST_PREPEND LIST_CONCAT
%token LIST_SET

%token EXCLAMATION
%token TILDE

%%

(* ==================== Operators ==================== *)

%public let core_unopt_infix ==
  | MINUS;                  { Operator.Neg }
  | EXCLAMATION;            { Operator.LogicalNot }
  | TILDE;                  { Operator.BitwiseNot }

%public let core_binopt_infix ==
  | PLUS;                   { Operator.Plus }
  | MINUS;                  { Operator.Minus }
  | TIMES;                  { Operator.Times }
  | DIVIDE;                 { Operator.Div }
  | MODULO;                 { Operator.Modulo }
  | POW;                    { Operator.Pow }
  | AMPERSAND;              { Operator.BitwiseAnd }
  | PIPE;                   { Operator.BitwiseOr }
  | CARET;                  { Operator.BitwiseXor }
  | SHIFT_LEFT;             { Operator.ShiftLeft }
  | SHIFT_RIGHT;            { Operator.ShiftRight }
  | SHIFT_RIGHT_LOGICAL;    { Operator.ShiftRightLogical }
  | LAND;                   { Operator.LogicalAnd }
  | LOR;                    { Operator.LogicalOr }
  | EQ;                     { Operator.Eq }
  | NE;                     { Operator.Ne }
  | LT;                     { Operator.Lt }
  | GT;                     { Operator.Gt }
  | LE;                     { Operator.Le }
  | GE;                     { Operator.Ge }

%public let core_unopt_call ==
  | INT_TO_FLOAT;           { Operator.IntToFloat }
  | INT_TO_STRING;          { Operator.IntToString }
  | FLOAT_TO_INT;           { Operator.FloatToInt }
  | FLOAT_TO_STRING;        { Operator.FloatToString }
  | STRING_TO_INT;          { Operator.StringToInt }
  | STRING_TO_FLOAT;        { Operator.StringToFloat }
  | FROM_CHAR_CODE;         { Operator.FromCharCode }
  | TO_CHAR_CODE;           { Operator.ToCharCode }
  | STRING_LEN;             { Operator.StringLen }
  | STRING_CONCAT;          { Operator.StringConcat }
  | LIST_HEAD;              { Operator.ListHead }
  | LIST_TAIL;              { Operator.ListTail }
  | LIST_LEN;               { Operator.ListLen }
  | LIST_REVERSE;           { Operator.ListReverse }

%public let core_binopt_call ==
  | STRING_NTH;             { Operator.StringNth }
  | LIST_NTH;               { Operator.ListNth }
  | LIST_ADD;               { Operator.ListAdd }
  | LIST_PREPEND;           { Operator.ListPrepend }
  | LIST_CONCAT;            { Operator.ListConcat }

%public let core_triopt ==
  | STRING_SUBSTR;          { Operator.StringSubstr }
  | LIST_SET;               { Operator.ListSet }
