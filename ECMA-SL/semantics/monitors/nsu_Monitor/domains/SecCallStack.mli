
type 'sl sf = |Intermediate of ('sl list * ('sl SecStore.t) * string)
  			  |Toplevel

type 'sl t 

val create : unit -> 'sl t 

val pop : ('sl t) -> (('sl sf) * ('sl t)) 

val push :  ('sl t) ->  ('sl sf)  -> ('sl t) 
