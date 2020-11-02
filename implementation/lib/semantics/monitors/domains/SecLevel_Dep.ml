exception Except of string
module SSet = Set.Make(String)
 
module M = struct

type t = SSet.t

type flow = t * t

let top  : t option ref = ref None

let flows : flow list ref = ref []

let all_levels : t list ref = ref []

let setTop (l_top : t): unit =
	top  := Some (l_top);
	()

let print_set (set : t) : unit =
	SSet.iter (fun str -> Printf.printf "# %s\n" str) set ;
	()

let get_low () : t =  SSet.empty 

let get_high () : t =
	match !top with
	| None -> raise (Except "SecLev_Dep was not initialized with top element.")
	| Some v -> v

let str (l:t) : string =
	"{" ^ (String.concat " " (SSet.elements l))   ^"}"

let flow_to_str (fl : flow) : string =
	match fl with 
	(fromset, toset) -> Printf.sprintf "%s -> %s" (str fromset) (str toset)

let apply_flow (fl : flow)  (lev: t) : t =
	Printf.printf "\tApplying flow %s to lev %s\n" (flow_to_str fl) (str lev);
	match fl with 
	(fromset, toset) ->
	 	if( SSet.subset toset lev) then SSet.union fromset lev
 		else lev
 	

let close_level (lev : t) : t =
	Printf.printf "Closing level %s\n" (str lev);
	let rec loop (chng : bool) (level : t) : t =
		let old_lev = level in 
		let lev2 =  List.fold_left (fun ac flow  ->  apply_flow flow ac) level !flows in
		let chng = not (SSet.equal level  old_lev) in
		if chng then (
			loop chng lev2 )
		else (
			
	 		lev2)
	in
	let res = loop true lev in 
		print_string ("\tResulting level: " ^ (str res ^ "\n"));
		res 
	


let lub (set1:t) (set2:t): t = 
	close_level (SSet.union set1 set2)
	  
let lubn (lst : t list): t =
  List.fold_left lub SSet.empty lst


let leq (set1:t) (set2:t): bool = 
	SSet.subset set1 set2 
	
let addFlow (set1: t) (set2 : t) : unit =
	flows := !flows @ [(set1, set2)] ;
	() 
	

let parse_lvl (str : string) : t =
	let rem1 = String.split_on_char '{' str in
    let rem2 = String.split_on_char '}' (List.nth rem1 1) in
    let finalst = String.split_on_char ',' (List.nth rem2 0) in
    if finalst = [""] then
    	close_level (SSet.of_list  [])
    else
	close_level (SSet.of_list finalst) 
end