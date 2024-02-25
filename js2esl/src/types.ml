type exit_value =
  [ `Success
  | `Error
  | `Parser_error of (Loc.t * Parse_error.t) list
  ]

let int_of_exit_value : exit_value -> int = function
  | `Success -> 0
  | `Error -> 1
  | `Parser_error _ -> 2
