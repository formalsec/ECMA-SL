let parse_prog (str : string) : Func.t list  =
	let lexbuf = Lexing.from_string str in
 		Parser.prog_target Lexer.read lexbuf



 let load_file f : string =
 	let ic = open_in f in
	 let n = in_channel_length ic in
 	let s = Bytes.create n in
 		really_input ic s 0 n;
 		close_in ic;
 		Bytes.to_string s

let parse_file str : Prog.t =
	let str = load_file str in
	let fs = parse_prog str in
	let prog = Prog.create_prog fs in 
	prog 	 


	let parse_error s = (* Called by the parser function on error *)
	   print_endline s;
	   flush stdout
