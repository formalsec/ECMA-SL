module type M = sig
  type t

  val create : string list -> t
  val empty : unit -> t
end

module SSet = Set.Make (String)

module M = struct
  include SSet

  type t = SSet.t

  let create (sl : string list) : t =
    List.fold_left (fun ac s -> SSet.add s ac) SSet.empty sl

  let empty () : t = SSet.empty
end
