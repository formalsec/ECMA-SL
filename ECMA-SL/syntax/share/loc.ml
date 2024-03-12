open EslBase

type t = string

let loc_id = Base.make_name_generator "$loc_"
let create () : t = loc_id ()
let pp (fmt : Fmt.t) (l : t) : unit = Fmt.fprintf fmt "%S" l
let str (l : t) : string = Fmt.asprintf "%a" pp l
