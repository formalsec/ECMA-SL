open EslBase

type t = int

let create : unit -> t =
  let (next, _) = Base.make_counter 0 1 in
  next

let equal (loc1 : t) (loc2 : t) : bool = loc1 == loc2 [@@inline]
let hash (loc : t) : int = loc [@@inline]

let pp (ppf : Format.formatter) (loc : t) : unit = Fmt.pf ppf "$loc_%d" loc
[@@inline]

let str (loc : t) : string = Fmt.str "%a" pp loc [@@inline]

module Tbl = struct
  (* TODO: Make this a Buffer *)
  include Hashtbl.Make (struct
    type t = int

    let equal (loc1 : t) (loc2 : t) : bool = equal loc1 loc2
    let hash (loc : t) : int = hash loc
  end)

  let pp (pp_sep : Format.formatter -> unit) (pp_v : ('a * 'b) Fmt.t)
    (ppf : Format.formatter) (loctbl : 'b t) =
    let iter f tbl = iter (fun k v -> f (k, v)) tbl in
    Fmt.iter iter ~sep:(fun fmt () -> pp_sep fmt) pp_v ppf loctbl

  let str (pp_sep : Format.formatter -> unit) (pp_v : ('a * 'b) Fmt.t) loctbl =
    Fmt.str "%a" (pp pp_sep pp_v) loctbl
  [@@inline]
end
