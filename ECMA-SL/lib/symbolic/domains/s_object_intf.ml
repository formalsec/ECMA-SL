module type SymbolicObject = sig
  type t
  type encoded_pct = Encoding.Expression.t

  val create : unit -> t
  val clone : t -> t
  val to_string : t -> (Expr.t -> string) -> string

  val set :
    t ->
    Expr.t ->
    Expr.t ->
    Encoding.Batch.t ->
    encoded_pct list ->
    S_store.t ->
    (t * encoded_pct list) list

  val get :
    t ->
    Expr.t ->
    Encoding.Batch.t ->
    encoded_pct list ->
    S_store.t ->
    (t * encoded_pct list * Expr.t option) list

  val delete :
    t ->
    Expr.t ->
    Encoding.Batch.t ->
    encoded_pct list ->
    S_store.t ->
    (t * encoded_pct list) list

  val to_list : t -> (Expr.t * Expr.t) list
  val has_field : t -> Expr.t -> Expr.t
  val get_fields : t -> Expr.t list
end
