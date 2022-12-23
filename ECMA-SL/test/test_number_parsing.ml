open Number_Parsing_Utils

let assert_equals x y =
  print_float x;
  print_float y;
  if x <> y then
    raise (Failure (Printf.sprintf "Expected: %.9f, but got %.9f." x y))

let test_parse_string_numeric_literal () =
  assert_equals (parse_string_numeric_literal "10.123214123") 10.123214123

let () = test_parse_string_numeric_literal ()
