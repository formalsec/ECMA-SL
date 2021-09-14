

let is_white_space (c : char) : bool = (c = ' ') || (c = '\n')


let rec parse_white_space_opt (str : string) (idx : int) : int = 
  if (is_white_space (String.get str idx))
    then parse_white_space_opt str (idx+1)
    else idx 


(*
StringNumericLiteral :::
  StrWhiteSpaceopt
  StrWhiteSpaceopt StrNumericLiteral StrWhiteSpaceopt
*)
let parser_string_numeric_literal (str : string) (idx : int) : float * int = 
  let idx' = parse_white_space_opt str idx in 
  if (idx' = String.length str) 
    then (0., idx')
    else (
      let (f, idx'') = parse_str_numeric_literal str idx' in 
      let idx''' = parse_white_space_opt str idx'' in 
      (f, idx''')
    )


(*
DecimalDigit ::: one of 
  0 1 2 3 4 5 6 7 8 9
*)
let parse_decimal_digit (str : string) (idx : int) : float option = 
  match String.get str idx with 
  | '0' -> Some 0. 
  | '1' -> Some 1. 
  | '2' -> Some 2. 
  | '3' -> Some 3. 
  | '4' -> Some 4. 
  | '5' -> Some 5. 
  | '6' -> Some 6. 
  | '7' -> Some 7. 
  | '8' -> Some 8. 
  | '9' -> Some 9. 
  | _   -> None 

(* 
   DecimalDigits :::
    DecimalDigit
    DecimalDigits DecimalDigit
*)
let rec parse_decimal_digits (str : string) (idx : int) (f : float) : (float * int) = 
  let od = parse_decimal_digit str idx in 
  match od with 
  | None   -> (f, idx)
  | Some d -> parse_decimal_digits str (idx+1) ((f*.10.) +. d)

(*
SignedInteger :::
    DecimalDigits
  + DecimalDigits
  - DecimalDigit

*)
let parse_signed_integer (str : string) (idx : int) : (float * int) = 
  let c = String.get str idx in 
  if (c = '-') then
    let (f, j) = parse_decimal_digits str (idx + 1) 0. in
    (f *. (-1.), j)
  else parse_decimal_digits str (idx + 1) 0.
