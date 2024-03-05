open Source

type t = t' Source.phrase
and t' = string

let default () : t = "" @> no_region
let equal (id1 : t) (id2 : t) : bool = String.equal id1.it id2.it
let pp (fmt : Fmt.t) (id : t) : unit = Fmt.fprintf fmt "%s" id.it
let str (id : t) : string = Fmt.asprintf "%a" pp id
