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
%token EQ NEQ LT GT LE GE

%token TYPEOF ITE

%token INT_TO_FLOAT INT_TO_STRING
%token FLOAT_TO_INT FLOAT_TO_STRING TO_INT32 TO_UINT16 TO_UINT32

%token STRING_TO_INT STRING_TO_FLOAT FROM_CHAR_CODE TO_CHAR_CODE
%token STRING_LEN STRING_CONCAT

%token STRING_NTH
%token STRING_SUBSTR

%token LIST_HEAD LIST_TAIL LIST_LEN LIST_REVERSE
%token LIST_NTH LIST_ADD LIST_PREPEND LIST_CONCAT
%token LIST_SET

%token TUPLE_FIRST TUPLE_SECOND TUPLE_LEN
%token TUPLE_NTH

%token RANDOM ABS SQRT CEIL FLOOR TRUNC EXP
%token MAX MIN

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
  | NEQ;                    { Operator.NE }
  | LT;                     { Operator.Lt }
  | GT;                     { Operator.Gt }
  | LE;                     { Operator.Le }
  | GE;                     { Operator.Ge }

%public let core_unopt_call ==
  | TYPEOF;                 { Operator.Typeof }
  | INT_TO_FLOAT;           { Operator.IntToFloat }
  | INT_TO_STRING;          { Operator.IntToString }
  | FLOAT_TO_INT;           { Operator.FloatToInt }
  | FLOAT_TO_STRING;        { Operator.FloatToString }
  | TO_INT32;               { Operator.ToInt32 }
  | TO_UINT16;              { Operator.ToUint16 }
  | TO_UINT32;              { Operator.ToUint32 }
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
  | TUPLE_FIRST;            { Operator.TupleFirst }
  | TUPLE_SECOND;           { Operator.TupleSecond }
  | TUPLE_LEN;              { Operator.TupleLen }
  | RANDOM;                 { Operator.Random }
  | ABS;                    { Operator.Abs }
  | SQRT;                   { Operator.Sqrt }
  | CEIL;                   { Operator.Ceil }
  | FLOOR;                  { Operator.Floor }
  | TRUNC;                  { Operator.Trunc }
  | EXP;                    { Operator.Exp }

%public let core_binopt_call ==
  | STRING_NTH;             { Operator.StringNth }
  | LIST_NTH;               { Operator.ListNth }
  | LIST_ADD;               { Operator.ListAdd }
  | LIST_PREPEND;           { Operator.ListPrepend }
  | LIST_CONCAT;            { Operator.ListConcat }
  | TUPLE_NTH;              { Operator.TupleNth }
  | MIN;                    { Operator.Min }
  | MAX;                    { Operator.Max }

%public let core_triopt ==
  | STRING_SUBSTR;          { Operator.StringSubstr }
  | LIST_SET;               { Operator.ListSet }
  | ITE;                    { Operator.ITE }
