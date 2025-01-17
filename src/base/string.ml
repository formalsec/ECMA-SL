include Stdlib.String

let substr ?(left : int option) ?(right : int option) (text : string) : string =
  let left' = Option.value ~default:0 left in
  let right' = Option.value ~default:(length text) right in
  sub text left' (right' - left')

let split_at_index (i : int) (text : string) : string * string =
  (substr ~right:i text, substr ~left:i text)

let rec split_with_len (len : int) (text : string) : string list =
  if length text > len then
    let (left, right) = split_at_index len text in
    left :: split_with_len len right
  else [ text ]

let truncate (limit : int) (text : string) : string * bool =
  let truncate_line line trunc =
    try if length line > limit then (sub line 0 limit, true) else (line, trunc)
    with Invalid_argument _ -> ("", true)
  in
  match split_on_char '\n' text with
  | [] -> ("", false)
  | line :: [] -> truncate_line line false
  | line :: _ -> truncate_line line true
