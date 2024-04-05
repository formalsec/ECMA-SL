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

%token TYPEOF

%token INT_TO_FLOAT INT_TO_STRING
%token FLOAT_TO_INT FLOAT_TO_STRING TO_INT TO_INT32 TO_UINT16 TO_UINT32 IS_NAN

%token STRING_TO_INT STRING_TO_FLOAT FROM_CHAR_CODE TO_CHAR_CODE 
%token STRING_LEN STRING_CONCAT

%token STRING_NTH STRING_NTH_U STRING_SPLIT
%token STRING_SUBSTR STRING_SUBSTR_U

%token ARRAY_MAKE
%token ARRAY_NTH
%token ARRAY_LEN
%token ARRAY_SET

%token LIST_TO_ARRAY LIST_HEAD LIST_TAIL LIST_LEN LIST_SORT LIST_REVERSE LIST_REMOVE_LAST
%token LIST_NTH LIST_ADD LIST_PREPEND LIST_CONCAT LIST_REMOVE LIST_REMOVE_NTH
%token LIST_MEM LIST_SET

%token TUPLE_FIRST TUPLE_SECOND TUPLE_LEN
%token TUPLE_NTH

%token FLOAT_TO_BYTE
%token FLOAT32_TO_LE_BYTES FLOAT32_TO_BE_BYTES FLOAT64_TO_LE_BYTES FLOAT64_TO_BE_BYTES
%token FLOAT32_FROM_LE_BYTES FLOAT32_FROM_BE_BYTES FLOAT64_FROM_LE_BYTES FLOAT64_FROM_BE_BYTES
%token BYTES_TO_STRING

%token INT_TO_BE_BYTES INT_FROM_LE_BYTES UINT_FROM_LE_BYTES

%token RANDOM ABS SQRT CEIL FLOOR TRUNC EXP LOG_2 LOG_E LOG_10
%token SIN COS TAN SINH COSH TANH ASIN ACOS ATAN
%token MAX MIN ATAN_2

%token UTF8_DECODE HEX_DECODE
%token PARSE_NUMBER PARSE_STRING PARSE_DATE

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
  | LIST_MEM;               { Operator.ListMem }

%public let core_unopt_call ==
  | TYPEOF;                 { Operator.Typeof }
  | INT_TO_FLOAT;           { Operator.IntToFloat }
  | INT_TO_STRING;          { Operator.IntToString }
  | FLOAT_TO_INT;           { Operator.FloatToInt }
  | FLOAT_TO_STRING;        { Operator.FloatToString }
  | TO_INT;                 { Operator.ToInt }
  | TO_INT32;               { Operator.ToInt32 }
  | TO_UINT16;              { Operator.ToUint16 }
  | TO_UINT32;              { Operator.ToUint32 }
  | IS_NAN;                 { Operator.IsNaN }
  | STRING_TO_INT;          { Operator.StringToInt }
  | STRING_TO_FLOAT;        { Operator.StringToFloat }
  | FROM_CHAR_CODE;         { Operator.FromCharCode }
  | TO_CHAR_CODE;           { Operator.ToCharCode }
  | STRING_LEN;             { Operator.StringLen }
  | STRING_CONCAT;          { Operator.StringConcat }
  | ARRAY_LEN;              { Operator.ArrayLen }
  | LIST_TO_ARRAY;          { Operator.ListToArray }
  | LIST_HEAD;              { Operator.ListHead }
  | LIST_TAIL;              { Operator.ListTail }
  | LIST_LEN;               { Operator.ListLen }
  | LIST_SORT;              { Operator.ListSort }
  | LIST_REVERSE;           { Operator.ListReverse }
  | LIST_REMOVE_LAST;       { Operator.ListRemoveLast }
  | TUPLE_FIRST;            { Operator.TupleFirst }
  | TUPLE_SECOND;           { Operator.TupleSecond }
  | TUPLE_LEN;              { Operator.TupleLen }
  | FLOAT_TO_BYTE;          { Operator.FloatToByte }
  | FLOAT32_TO_LE_BYTES;    { Operator.Float32ToLEBytes }
  | FLOAT32_TO_BE_BYTES;    { Operator.Float32ToBEBytes }
  | FLOAT64_TO_LE_BYTES;    { Operator.Float64ToLEBytes }
  | FLOAT64_TO_BE_BYTES;    { Operator.Float64ToBEBytes }
  | FLOAT32_FROM_LE_BYTES;  { Operator.Float32FromLEBytes }
  | FLOAT32_FROM_BE_BYTES;  { Operator.Float32FromBEBytes }
  | FLOAT64_FROM_LE_BYTES;  { Operator.Float64FromLEBytes }
  | FLOAT64_FROM_BE_BYTES;  { Operator.Float64FromBEBytes }
  | BYTES_TO_STRING;        { Operator.BytesToString }
  | RANDOM;                 { Operator.Random }
  | ABS;                    { Operator.Abs }
  | SQRT;                   { Operator.Sqrt }
  | CEIL;                   { Operator.Ceil }
  | FLOOR;                  { Operator.Floor }
  | TRUNC;                  { Operator.Trunc }
  | EXP;                    { Operator.Exp }
  | LOG_2;                  { Operator.Log2 }
  | LOG_E;                  { Operator.LogE }
  | LOG_10;                 { Operator.Log10 }
  | SIN;                    { Operator.Sin }
  | COS;                    { Operator.Cos }
  | TAN;                    { Operator.Tan }
  | SINH;                   { Operator.Sinh }
  | COSH;                   { Operator.Cosh }
  | TANH;                   { Operator.Tanh }
  | ASIN;                   { Operator.Asin }
  | ACOS;                   { Operator.Acos }
  | ATAN;                   { Operator.Atan }
  | UTF8_DECODE;            { Operator.Utf8Decode }
  | HEX_DECODE;             { Operator.HexDecode }
  | PARSE_NUMBER;           { Operator.ParseNumber }
  | PARSE_STRING;           { Operator.ParseString }
  | PARSE_DATE;             { Operator.ParseDate }

%public let core_binopt_call ==
  | STRING_NTH;             { Operator.StringNth }
  | STRING_NTH_U;           { Operator.StringNthU }
  | STRING_SPLIT;           { Operator.StringSplit }
  | ARRAY_MAKE;             { Operator.ArrayMake }
  | ARRAY_NTH;              { Operator.ArrayNth }
  | LIST_NTH;               { Operator.ListNth }
  | LIST_ADD;               { Operator.ListAdd }
  | LIST_PREPEND;           { Operator.ListPrepend }
  | LIST_CONCAT;            { Operator.ListConcat }
  | LIST_REMOVE;            { Operator.ListRemove }
  | LIST_REMOVE_NTH;        { Operator.ListRemoveNth }
  | TUPLE_NTH;              { Operator.TupleNth }
  | INT_TO_BE_BYTES;        { Operator.IntToBEBytes }
  | INT_FROM_LE_BYTES;      { Operator.IntFromLEBytes }
  | UINT_FROM_LE_BYTES;     { Operator.UintFromLEBytes }
  | MIN;                    { Operator.Min }
  | MAX;                    { Operator.Max }
  | ATAN_2;                 { Operator.Atan2 }

%public let core_triopt ==
  | STRING_SUBSTR;          { Operator.StringSubstr }
  | STRING_SUBSTR_U;        { Operator.StringSubstrU }
  | ARRAY_SET;              { Operator.ArraySet }
  | LIST_SET;               { Operator.ListSet }
