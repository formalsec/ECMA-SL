open EslBase

type t = int

let create : unit -> t =
  let (next, _) = Base.make_counter 0 1 in
  next

let equal (l1 : t) (l2 : t) : bool = l1 == l2 [@@inline]
let hash (l : t) : int = l [@@inline]
let pp (ppf : Fmt.t) (l : t) : unit = Fmt.fmt ppf "$loc_%d" l [@@inline]
let str (l : t) : string = Fmt.str "%a" pp l [@@inline]

module Tbl = struct
  include Hashtbl.Make (struct
    type t = int

    let equal (l1 : t) (l2 : t) : bool = equal l1 l2
    let hash (l : t) : int = hash l
  end)

  let pp (pp_sep : Fmt.t -> unit) (pp_v : Fmt.t -> 'a * 'b -> unit)
    (ppf : Fmt.t) (loctbl : 'b t) =
    let iter_f pp_f tbl = iter (fun a b -> pp_f (a, b)) tbl in
    Fmt.pp_iter pp_sep iter_f pp_v ppf loctbl

  let str (pp_sep : Fmt.t -> unit) (pp_v : Fmt.t -> 'a * 'b -> unit)
    (loctbl : 'b t) : string =
    Fmt.str "%a" (pp pp_sep pp_v) loctbl
  [@@inline]
end
