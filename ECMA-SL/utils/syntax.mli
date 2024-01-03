module Option : sig
  val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
  val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option
end

module Result : sig
  val ( let* ) :
    ('a, 'c) Result.t -> ('a -> ('b, 'c) Result.t) -> ('b, 'c) Result.t

  val ( let+ ) : ('a, 'c) Result.t -> ('a -> 'b) -> ('b, 'c) Result.t

  val list_map :
    f:('a -> ('b, string) Result.t) -> 'a list -> ('b list, string) Result.t
end
