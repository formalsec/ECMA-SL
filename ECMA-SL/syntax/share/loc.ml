open Core

type t = String.t

let count = ref 0

let inc_get_count () : int =
  incr count;
  !count

let newloc () : t = "$loc_" ^ string_of_int (inc_get_count ())
let pp fmt v = Format.fprintf fmt {|"%s"|} v
let str (v : t) : string = Format.asprintf "%a" pp v
let parse_loc (s : t) : t option = Some s
