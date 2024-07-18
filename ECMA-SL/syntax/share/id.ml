open EslBase
open Source

type t = t' Source.t
and t' = string

let default : unit -> t =
  let dflt = "" @> none in
  fun () -> dflt

let equal (id1 : t) (id2 : t) : bool = String.equal id1.it id2.it [@@inline]
let pp (ppf : Fmt.t) (id : t) : unit = Fmt.fmt ppf "%s" id.it [@@inline]
let str (id : t) : string = Fmt.str "%a" pp id [@@inline]
