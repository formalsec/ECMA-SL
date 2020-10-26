exception Except of string
module SSet = Set.Make(String)
 
module M = struct

type t = SSet.t

type flow = t * t

let top  : SSet.t option ref = ref None

let flows : flow list ref = ref []

let setTop (l_top : string list): unit =
	top  := Some (SSet.of_list l_top);
	()

let get_low () : t =  SSet.empty 

let get_high () : t =
	match !top with
	| None -> raise (Except "SecLev_Dep was not initialized with top element.")
	| Some v -> v

let lub (set1:t) (set2:t): t = SSet.union set1 set2
	  
let lubn (lst : t list): t =
  List.fold_left lub SSet.empty lst


let leq (set1:t) (set2:t): bool = SSet.subset set1 set2 
	


let str (l:t) : string =
	"{" ^ (String.concat " " (SSet.elements l))   ^"}"

let addFlow (lst1: string list) (lst2 : string list) : unit =
	let set1, set2 = (SSet.of_list lst1), (SSet.of_list lst2) in 
	flows := !flows @ [(set1, set2)] ; 
	()

let apply_flow (fl : flow)  (lev: t) : t =
	match fl with 
	(fromset, toset) -> if( SSet.subset toset lev) then SSet.union fromset lev
 		else lev
 	

let close_level (lev : t) : unit =
	let rec loop (chng : bool) (level : t) : unit =
		let old_lev = level in 
		let lev2 =  List.fold_left (fun ac flow  ->  apply_flow flow ac) level !flows in
		let chng = not (SSet.equal level  old_lev) in
		if chng then 
			loop chng lev2 
		else
	 		()
	in
	loop true lev; 
	()





let parse_lvl (str : string) : t =
	SSet.empty 


  

end