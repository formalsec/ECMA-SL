(*
StrWhiteSpaceChar :::
WhiteSpace
LineTerminator
*)
let is_white_space (c : char) : bool = c = ' ' || c = '\n'

(*
StrWhiteSpace :::
StrWhiteSpaceChar StrWhiteSpaceopt
*)
let rec parse_white_space_opt (str : string) (idx : int) : int =
  Printf.printf "parse_white_space_opt %s %d\n" str idx;
  if String.length str > idx && is_white_space (String.get str idx) then
    parse_white_space_opt str (idx + 1)
  else idx

(*
BinaryIntegerLiteral ::
  0b BinaryDigits
  0B BinaryDigits
*)
let is_binary_digit (c : char) : bool = c = '0' || c = '1'

let rec advance_binary_digits (str : string) (idx : int) : int =
  if String.length str > idx && is_binary_digit (String.get str idx) then
    advance_binary_digits str (idx + 1)
  else idx

let parse_binary_integer_literal (str : string) (idx : int) : float * int =
  Printf.printf "parse_binary_integer_literal %s %d\n" str idx;
  let len = String.length str in
  (Int64.to_float (Int64.of_string ("0b" ^ str)), idx + len - 1)

(*
OctalIntegerLiteral ::
  0o OctalDigits
  0O OctalDigits
*)
let is_octal_digit (c : char) : bool = 48 <= Char.code c && Char.code c <= 55

let rec advance_octal_digits (str : string) (idx : int) : int =
  if String.length str > idx && is_octal_digit (String.get str idx) then
    advance_octal_digits str (idx + 1)
  else idx

let parse_octal_integer_literal (str : string) (idx : int) : float * int =
  Printf.printf "parse_octal_integer_literal %s %d\n" str idx;
  let len = String.length str in
  (Int64.to_float (Int64.of_string ("0o" ^ str)), idx + len - 1)

(*
   HexIntegerLiteral ::
     0x HexDigits
     0X HexDigits
*)
let parse_hex_integer_literal (str : string) (idx : int) : float * int =
  Printf.printf "parse_hex_integer_literal %s %d\n" str idx;
  let len = String.length str in
  (Int64.to_float (Int64.of_string ("0x" ^ str)), idx + len - 1)

let parse_infinity (str : string) (idx : int) : (float * int) option =
  if String.length str - idx >= 8 then
    if String.sub str idx 8 = "Infinity" then Some (Float.infinity, idx + 8)
    else None
  else None

(*
DecimalDigit ::: one of
  0 1 2 3 4 5 6 7 8 9
*)
let parse_decimal_digit (str : string) (idx : int) : float option =
  Printf.printf "parse_decimal_digit %s %d\n" str idx;
  let length = String.length str in
  Printf.printf "parse_decimal_digit %d\n" length;
  if idx >= length then (
    Printf.printf "%s\n" "None";
    None )
  else
    let code = Char.code (String.get str idx) in
    if 47 < code && code < 58 then Some (Float.of_int (code - 48)) else None

(*
DecimalDigits :::
  DecimalDigit
  DecimalDigits DecimalDigit
*)
let rec parse_decimal_digits (str : string) (idx : int) (f : float) :
  float * int =
  Printf.printf "parse_decimal_digits %s %d %f\n" str idx f;
  let od = parse_decimal_digit str idx in
  match od with
  | None -> (f, idx)
  | Some d -> parse_decimal_digits str (idx + 1) ((f *. 10.) +. d)

(*
ExponentIndicator :: one of
  e E
*)
let is_exponent_indicator (c : char) : bool = c = 'e' || c = 'E'

(*
SignedInteger :::
  DecimalDigits
  + DecimalDigits
  - DecimalDigits
*)
let parse_signed_integer (str : string) (idx : int) : float * int =
  Printf.printf "parse_signed_integer %s %d\n" str idx;
  let c = String.get str idx in
  if c = '-' then
    let (f, idx') = parse_decimal_digits str (idx + 1) 0. in
    (f *. -1., idx')
  else parse_decimal_digits str (idx + 1) 0.

(*
ExponentPart :::
ExponentIndicator SignedInteger
*)
let parse_exponent_part (str : string) (idx : int) : float * int =
  Printf.printf "parse_exponent_part %s %d\n" str idx;
  let c = String.get str idx in
  if is_exponent_indicator c then parse_signed_integer str (idx + 1)
  else (0., idx + 1)

(*
StrUnsignedDecimalLiteral :::
Infinity
. DecimalDigits ExponentPartopt
DecimalDigits . DecimalDigitsopt ExponentPartopt
DecimalDigits ExponentPartopt
*)
let parse_str_unsigned_decimal_literal (str : string) (idx : int) : float * int
    =
  Printf.printf "parse_str_unsigned_decimal_literal %s %d\n" str idx;
  let res = parse_infinity str idx in
  match res with
  | Some (f, idx') -> (f, idx')
  | None ->
    let c = String.get str idx in
    if c = '.' then
      let (f1, idx1) = parse_decimal_digits str (idx + 1) 0. in
      let (f2, idx2) = parse_exponent_part str idx1 in
      let exp = -.(Int.to_float (String.length (Float.to_string f1)) -. 1.) in
      (f1 *. (10. ** exp) *. (10. ** f2), idx2)
    else
      let (f1, idx1) = parse_decimal_digits str idx 0. in
      if idx1 >= String.length str then (f1, idx1)
      else
        let c = String.get str idx1 in
        if c = '.' then
          let (f2, idx2) = parse_decimal_digits str (idx1 + 1) 0. in
          let (f3, idx3) = parse_exponent_part str idx2 in
          let exp =
            -.(Int.to_float (String.length (Float.to_string f2)) -. 1.)
          in
          ((f1 +. (f2 *. (10. ** exp))) *. (10. ** f3), idx3)
        else
          let (f2, idx2) = parse_exponent_part str (idx1 + 1) in
          (f1 *. (10. ** f2), idx2)

(*
StrDecimalLiteral :::
StrUnsignedDecimalLiteral
+ StrUnsignedDecimalLiteral
- StrUnsignedDecimalLiteral
*)
let parse_str_decimal_literal (str : string) (idx : int) : float * int =
  Printf.printf "parse_str_decimal_literal %s %d\n" str idx;
  let c = String.get str idx in
  if c = '-' then
    let (f, idx') = parse_str_unsigned_decimal_literal str (idx + 1) in
    (f *. -1., idx')
  else parse_str_unsigned_decimal_literal str idx

(*
StrNumericLiteral :::
BinaryIntegerLiteral
OctalIntegerLiteral
HexIntegerLiteral
StrDecimalLiteral
*)
let parse_str_numeric_literal (str : string) (idx : int) : float * int =
  Printf.printf "parse_str_numeric_literal %s %d\n" str idx;
  let c1 = String.get str idx in
  if c1 = '0' && String.length str >= idx + 2 then
    match Char.lowercase_ascii (String.get str (idx + 1)) with
    | 'b' -> parse_binary_integer_literal str (idx + 2)
    | 'o' -> parse_octal_integer_literal str (idx + 2)
    | 'x' -> parse_hex_integer_literal str (idx + 2)
    | _ -> parse_str_decimal_literal str idx
  else parse_str_decimal_literal str idx

(*
StringNumericLiteral :::
  StrWhiteSpaceopt
  StrWhiteSpaceopt StrNumericLiteral StrWhiteSpaceopt
*)
let parse_string_numeric_literal (str : string) : float =
  Printf.printf "parse_string_numeric_literal %s\n" str;
  let idx1 = parse_white_space_opt str 0 in
  if idx1 = String.length str then 0.
  else
    let (f, idx2) = parse_str_numeric_literal str idx1 in
    let _ = parse_white_space_opt str idx2 in
    f
