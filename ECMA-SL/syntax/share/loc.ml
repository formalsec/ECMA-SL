open EslBase

type t = int

let create : unit -> t =
  let (next, _) = Base.make_counter 0 1 in
  next

let equal (loc1 : t) (loc2 : t) : bool = loc1 == loc2 [@@inline]
let hash (loc : t) : int = loc [@@inline]
let pp (ppf : Fmt.t) (loc : t) : unit = Fmt.fmt ppf "$loc_%d" loc
let str (loc : t) : string = Fmt.str "%a" pp loc

module Tbl = struct
  include Hashtbl.Make (struct
    type t = int

    let equal (loc1 : t) (loc2 : t) : bool = equal loc1 loc2
    let hash (loc : t) : int = hash loc
  end)

  let pp (pp_sep : Fmt.t -> unit) (pp_v : Fmt.t -> 'a * 'b -> unit)
    (ppf : Fmt.t) (loctbl : 'b t) =
    let iter_f f tbl = iter (fun a b -> f (a, b)) tbl in
    Fmt.pp_iter pp_sep iter_f pp_v ppf loctbl

  let str (pp_sep : Fmt.t -> unit) (pp_v : Fmt.t -> 'a * 'b -> unit)
    (loctbl : 'b t) : string =
    Fmt.str "%a" (pp pp_sep pp_v) loctbl
end
