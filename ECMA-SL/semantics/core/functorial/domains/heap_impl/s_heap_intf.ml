module type SymbolicHeap = sig
  type encoded_pct = Encoding.Expression.t
  type obj
  type t

  val create : unit -> t
  val clone : t -> t
  val insert : t -> obj -> Loc.t
  val remove : t -> Loc.t -> unit
  val set : t -> Loc.t -> obj -> unit
  val get : ?setVal:bool -> t -> Loc.t -> obj option

  val assign_obj_fields :
    t -> Expr.t -> Batch.t -> encoded_pct list -> S_store.t -> Expr.t

  val assign_obj_to_list :
    t -> Expr.t -> Batch.t -> encoded_pct list -> S_store.t -> Expr.t

  val has_field :
       t
    -> Expr.t
    -> Expr.t
    -> Batch.t
    -> encoded_pct list
    -> S_store.t
    -> (t * encoded_pct list * Expr.t) list

  val get_field :
       t
    -> Expr.t
    -> Expr.t
    -> Batch.t
    -> encoded_pct list
    -> S_store.t
    -> (t * encoded_pct list * Expr.t option) list

  val set_field :
       t
    -> Expr.t
    -> Expr.t
    -> Expr.t
    -> Batch.t
    -> encoded_pct list
    -> S_store.t
    -> (t * encoded_pct list) list

  val delete_field :
       t
    -> Expr.t
    -> Expr.t
    -> Batch.t
    -> encoded_pct list
    -> S_store.t
    -> (t * encoded_pct list) list
end
