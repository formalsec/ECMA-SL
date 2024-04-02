include Stdlib.String

let substr ?(left : int option) ?(right : int option) (text : string) : string =
  let left' = Option.value ~default:0 left in
  let right' = Option.value ~default:(length text) right in
  sub text left' (right' - left')

let ordinal_suffix (n : int) : string =
  let suffix =
    if n mod 100 / 10 = 1 then "th"
    else match n mod 10 with 1 -> "st" | 2 -> "nd" | 3 -> "rd" | _ -> "th"
  in
  string_of_int n ^ suffix

let truncate (limit : int) (text : string) : string * bool =
  let truncate_line line trunc =
    try if length line > limit then (sub line 0 limit, true) else (line, trunc)
    with Invalid_argument _ -> ("", true)
  in
  match split_on_char '\n' text with
  | [] -> ("", false)
  | line :: [] -> truncate_line line false
  | line :: _ -> truncate_line line true
