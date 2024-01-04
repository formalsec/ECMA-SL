type t = string

let count = ref 0

let inc_get_count () : int =
  incr count;
  !count

let create () : t = Printf.sprintf "$loc_%d" (inc_get_count ())
let pp (fmt : Format.formatter) (l : t) : unit = Format.fprintf fmt "%S" l
let str (l : t) : string = Format.asprintf "%a" pp l
