open Parsing_number_utils

let assert_equals x y =
  if x <> y then
    raise (Failure (Printf.sprintf "Expected: %.9f, but got %.9f." x y))

let test_is_white_space () =
  assert (is_white_space ' ');
  assert (is_white_space '\n');
  assert (is_white_space '\t')

let test_parse_string_numeric_literal () =
  assert_equals (parse_string_numeric_literal "10.123214123") 10.123214123

let () =
  test_is_white_space ();
  test_parse_string_numeric_literal ()
