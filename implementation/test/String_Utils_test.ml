(*
	OUnit tutorials:
	- https://www.cs.cornell.edu/courses/cs3110/2020sp/textbook/data/ounit.html
	- https://medium.com/@bobbypriambodo/using-ounit-to-test-your-ocaml-program-ce8e640a828c

	Install OUnit:
	$ opam install ounit
*)

open OUnit2
open String_Utils

let tests =
  "Unicode" >::: [
    "s_len_u - unicode escape length"  >:: (fun _ ->
      assert_equal 4 (s_len_u "\u{00FF}\u{00FF}\u{00FF}\u{00FF}")
    );
	  "s_len_u - decimal escape length"  >:: (fun _ ->
      assert_equal 4 (s_len_u "\255\255\255\u{00FF}")
    );
	  "s_len_u - unicode as decimal length"  >:: (fun _ ->
      assert_equal 4 (s_len_u "\195\191\255\255\195\191")
    );
	  "s_nth_u - unicode escape nth"  >:: (fun _ ->
      assert_equal "\u{1234}" (s_nth_u "\u{00FF}\u{00FF}\u{1234}\u{00FF}" 2)
    );
	  "s_nth_u - decimal and hexadecimal escape nth"  >:: (fun _ ->
      assert_equal "\u{1234}" (s_nth_u "\u{00FF}\255\xe1\x88\xb4\255" 2)
    );
  	"to_char_code_u - unicode escape to int"  >:: (fun _ ->
      assert_equal 255 (to_char_code_u "\u{00FF}")
    );
	  "to_char_code_u - decimal escape to int"  >:: (fun _ ->
      assert_equal 255 (to_char_code_u "\255")
    );
	  "to_char_code_u - unicode as decimal escape to int"  >:: (fun _ ->
      assert_equal 255 (to_char_code_u "\195\191")
    );
	  "to_char_code_u - unicode larger than 255 to int"  >:: (fun _ ->
      assert_equal 4660 (to_char_code_u "\u{1234}")
    );
	  "to_char_code_u - unicode as hexadecimal escape, larger than 255, to int"  >:: (fun _ ->
      assert_equal 4660 (to_char_code_u "\xe1\x88\xb4")
    );
    (* Note that "\195\191" is considered different than "\255" *)
    "from_char_code_u - int to string"  >:: (fun _ ->
      assert_equal "\195\191" (from_char_code_u 255)
    );
	  "from_char_code_u - int larger than 255 to string"  >:: (fun _ ->
      assert_equal "\u{1234}" (from_char_code_u 4660)
    );
	  "s_substr_u - unicode escape substring"  >:: (fun _ ->
      assert_equal "\u{1234}\u{00FF}" (s_substr_u "\u{00FF}\u{00FF}\u{1234}\u{00FF}" 2 2)
    );
	  "s_substr_u - unicode escape substring"  >:: (fun _ ->
      assert_equal "\u{1234}\255" (s_substr_u "\u{00FF}\u{00FF}\xe1\x88\xb4\255" 2 2)
    );
	  "s_substr_u - unicode escape substring"  >:: (fun _ ->
      assert_equal "\u{1234}\u{00FF}" (s_substr_u "\u{00FF}\u{00FF}\u{1234}\195\191" 2 2)
    );
    "utf8decode - \\u1234"  >:: (fun _ ->
      assert_equal "\u{1234}" (utf8decode "\\u{1234}")
    );
    (* Ocaml does not support high-surrogate code points: http://unicode.org/glossary/#unicode_scalar_value *)
    "utf8decode and to_char_code_u - \\u1234"  >:: (fun _ ->
      assert_equal 4660 (to_char_code_u (utf8decode "\\u{1234}"))
    );
    "high-surrogate - \\uD834"  >:: (fun _ ->
      assert_equal 55348 (to_char_code_u (utf8decode "\\u{D834}"))
    );
    "hexdecode - \\x20"  >:: (fun _ ->
      assert_equal "\x20" (hexdecode "\\x20")
    );
    "utf8decode - \\u10FFFF"  >:: (fun _ ->
      assert_equal "\u{10FFFF}" (utf8decode "\\u{10FFFF}")
    );
    "utf8decode and to_char_code_u - \\u10FFFF"  >:: (fun _ ->
      assert_equal 1114111 (to_char_code_u (utf8decode "\\u{10FFFF}"))
    );
    "utf8decode - \\u10000"  >:: (fun _ ->
      assert_equal "\u{10000}" (utf8decode "\\u{10000}")
    );
    "utf8decode - 0x10000"  >:: (fun _ ->
      assert_equal 1114111 0x10FFFF
    );
  ]

let _ = run_test_tt_main tests