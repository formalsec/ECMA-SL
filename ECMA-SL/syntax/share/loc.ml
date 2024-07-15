open EslBase

type t = int

let create : unit -> t =
  let (next, _) = Base.make_counter 0 1 in
  next

let equal (l1 : t) (l2 : t) : bool = l1 == l2 [@@inline]
let hash (l : t) : int = l [@@inline]
let pp (ppf : Fmt.t) (l : t) : unit = Fmt.fmt ppf "$loc_%d" l
let str (l : t) : string = Fmt.str "%a" pp l

module Tbl = struct
  include Hashtbl.Make (struct
    type t = int

    let equal (x1 : t) (x2 : t) : bool = equal x1 x2
    let hash (x : t) : int = hash x
  end)

  let pp (pp_sep : Fmt.t -> unit) (pp_el : Fmt.t -> 'a * 'b -> unit)
    (ppf : Fmt.t) (tbl : 'b t) =
    let tbl_iter_f f tbl = iter (fun a b -> f (a, b)) tbl in
    Fmt.(pp_iter pp_sep tbl_iter_f pp_el ppf tbl)

  let str (pp_sep : Fmt.t -> unit) (pp_el : Fmt.t -> 'a * 'b -> unit)
    (tbl : 'b t) =
    Fmt.str "%a" (pp pp_sep pp_el) tbl
end
