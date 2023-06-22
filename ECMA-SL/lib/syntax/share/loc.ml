open Core
type t = String.t

let count = ref 0

let inc_get_count () : int =
  incr count;
  !count

let newloc () : t = "$loc_" ^ string_of_int (inc_get_count ())
let str (v : t) : string = "\"" ^ v ^ "\""
let parse_loc (s : t) : t option = Some s
