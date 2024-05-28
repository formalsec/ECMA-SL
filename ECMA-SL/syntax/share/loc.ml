open EslBase

type t = int

let create : unit -> t =
  let (next, _) = Base.make_counter 0 1 in
  next

let equal l1 l2 = l1 == l2 [@@inline]
let hash l = l [@@inline]
let pp (ppf : Fmt.t) (l : t) : unit = Fmt.format ppf "$loc_%d" l
let str (l : t) : string = Fmt.str "%a" pp l

module Tbl = struct
  include Hashtbl.Make (struct
    type t = int

    let equal x1 x2 = equal x1 x2
    let hash x = hash x
  end)

  let pp (sep : string) (pp_el : Fmt.t -> 'a * 'b -> unit) (ppf : Fmt.t)
    (tbl : 'b t) =
    let tbl_iter f tbl = iter (fun a b -> f (a, b)) tbl in
    Fmt.(pp_iter (!>"%s" sep) tbl_iter pp_el ppf tbl)
end
