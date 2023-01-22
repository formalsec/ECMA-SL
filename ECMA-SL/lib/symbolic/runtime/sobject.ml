module Symbolic_object = Map.Make (String)

type 'a t = 'a Symbolic_object.t

let create () : 'a t = Symbolic_object.empty
let find_opt (o : 'a t) (f : Field.t) : 'a option = Symbolic_object.find_opt f o
let add (o : 'a t) (f : Field.t) (v : 'a) : 'a t = Symbolic_object.add f v o

let add_opt (o : 'a t option) (f : Field.t) (v : 'a) : 'a t option =
  Option.map_default (fun o -> Some (add o f v)) None o

let remove (o : 'a t) (f : Field.t) : 'a t = Symbolic_object.remove f o

let remove_opt (o : 'a t option) (f : Field.t) : 'a t option =
  Option.map_default (fun o -> Some (remove o f)) None o

let to_list (o : 'a t) : (Field.t * 'a) list = Symbolic_object.bindings o

let get_fields (o : 'a t) : Field.t list =
  List.map (fun (f, _) -> f) (to_list o)
