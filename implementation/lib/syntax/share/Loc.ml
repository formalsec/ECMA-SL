type t = string

let loc_prefix = "$loc_" 

let loc_prefix_len = String.length loc_prefix 

let count = ref 0

let inc_get_count () : int = incr count; !count

let newloc () : t =
  loc_prefix ^ string_of_int (inc_get_count ())

let str (v : t) : string = "\"" ^ v ^ "\""

let parse_loc (s : string) : t option = 
  let s_len = String.length s in 
  if (s_len <= loc_prefix_len)
    then None 
    else if (String.sub s 0 loc_prefix_len) = loc_prefix 
      then Some s 
      else None  
  