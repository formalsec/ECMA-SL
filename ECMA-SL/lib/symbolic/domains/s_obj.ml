open Core
module type SymbolicObject = sig
  type 'a t
  type encoded_pct = Encoding.Expression.t

  val create : unit -> 'a t
  val clone : 'a t -> 'a t
  val to_string : 'a t -> ('a -> string) -> string

  val set : 'a t -> Expr.t -> 'a -> Encoding.Batch.t -> encoded_pct list -> Sstore.t -> ('a t * encoded_pct list) list
  val get : 'a t -> Expr.t -> Encoding.Batch.t -> encoded_pct list -> Sstore.t -> ('a t * encoded_pct list * 'a option) list
  val delete : 'a t -> Expr.t -> Encoding.Batch.t -> encoded_pct list -> Sstore.t -> ('a t * encoded_pct list) list

  val to_list : 'a t -> (String.t * 'a) list
  val get_fields : 'a t -> Expr.t list
end

(* open Core
module type SymbolicObject = sig
  type 'a t
  type encoded_pct = Encoding.Expression.t

  val create : unit -> 'a t
  val clone : 'a t -> 'a t
  val to_string : 'a t -> ('a -> string) -> string

  val set : 'a t -> Expr.t -> 'a -> Encoding.Batch.t -> encoded_pct list -> Sstore.t -> ('a t * encoded_pct list) list
  val get : 'a t -> Expr.t -> Encoding.Batch.t -> encoded_pct list -> Sstore.t -> ('a t * encoded_pct list * 'a option) list
  val delete : 'a t -> Expr.t -> Encoding.Batch.t -> encoded_pct list -> Sstore.t -> ('a t * encoded_pct list) list

  val to_list : 'a t -> (String.t * 'a) list
  val get_fields : 'a t -> Expr.t list
end *)