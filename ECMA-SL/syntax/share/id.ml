open Source

type t = t' Source.phrase
and t' = string

let default () : t = ?@""
let pp (fmt : Fmt.t) (id : t) : unit = Fmt.fprintf fmt "%s" id.it
let str (id : t) : string = Fmt.asprintf "%a" pp id
