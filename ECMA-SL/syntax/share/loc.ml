type t = string

let count = ref 0

let inc_get_count () : int =
  incr count;
  !count

let create () : t = Fmt.sprintf "$loc_%d" (inc_get_count ())
let pp (fmt : Fmt.formatter) (l : t) : unit = Fmt.fprintf fmt "%S" l
let str (l : t) : string = Fmt.asprintf "%a" pp l
