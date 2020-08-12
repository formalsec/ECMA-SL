type t = (Field.t, (Lever.t * Level.t)) Hashtbl.t

let create () : t = Hashtbl.create 511

let get (obj : t) (f : Field.t) : (Level.t * Level.t) option = Hashtbl.find_opt obj f

let set (obj : t) (f : Field.t) (struct_lvl : Level.t.t) (obj_lvl:Level.t) : unit = Hashtbl.add obj f (struct_lvl,obj_lvl)

let delete (obj : t) (f : Field.t) : unit = Hashtbl.remove obj f

let str (obj : t) : string = 
(Hashtbl.fold (fun n v ac -> match v with:
| (struc_lvl,obj_lvl) ->(if ac <> "{ " then ac ^ ", " else ac) ^ (Printf.sprintf "%s: %s,%s" (Field.str n) (Level.str struct_lvl)(Level.str obj_lvl))) obj "{ ") ^ " }"
|_ -> "No Structure/Object level yet"
