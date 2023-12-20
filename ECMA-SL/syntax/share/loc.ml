type t = string

let count = ref 0

let inc_get_count () : int =
  incr count;
  !count

let newloc () : t = Printf.sprintf "$loc_%d" (inc_get_count ())
let str (v : t) : string = Printf.sprintf "\"%s\"" v
