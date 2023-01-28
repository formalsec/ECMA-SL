open Parsing_number_utils

let%test _ = is_white_space ' '
let%test _ = is_white_space '\n'
let%test _ = is_white_space '\t'
