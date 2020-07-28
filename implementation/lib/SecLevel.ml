exception Except of string

type t=
| High
| Low
| Empty

let str (l:t) =
	match l with
	| High -> "\"high\""
	| Low -> "\"low\""
	| Empty -> "Empty"


let parse_lvl (str:string) : t =
	if (str = "\"low\"") then Low
			else if(str = "\"high\"") then High
			else if (str = "") then Empty
		else raise(Except ("Unknown Level -"^str))


let lub (s1:string) (s2:string): string =
	let l1 =  parse_lvl s1 in
	let l2 = parse_lvl s2 in
	match l1 with
	| High -> "\"high\""
	| Low -> (match l2 with
			    | High -> "\"high\""
					| Low -> "\"low\""
					|_ -> raise (Except "Invalid Level use")
					;)
	|_ -> raise (Except "Invalid Level use")



let leq (s1:string) (s2:string): bool =
	let l1 =  parse_lvl s1 in
	let l2 = parse_lvl s2 in
	match l1 with
	|High->true
	|Low -> (match l2 with
			| High -> false
			|Low -> true
			|_ -> raise (Except "Invalid Level use"))
	|_ -> raise (Except "Invalid Level use")


let str (l:t) =
	match l with
	| High -> "\"high\""
	| Low -> "\"low\""
	| Empty -> "Empty"
