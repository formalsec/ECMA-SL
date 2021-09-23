open Number_Parsing_Utils

let main () =
	let num = parse_string_numeric_literal "10.123214123" in
	Printf.printf "%f\n" num
	;;

main ()