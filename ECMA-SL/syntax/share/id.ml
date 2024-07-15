open EslBase

type t = t' Source.t
and t' = string

let default () : t = Source.("" @> none)
let equal (id1 : t) (id2 : t) : bool = String.equal id1.it id2.it
let pp (ppf : Fmt.t) (id : t) : unit = Fmt.fmt ppf "%s" id.it
let str (id : t) : string = Fmt.str "%a" pp id
