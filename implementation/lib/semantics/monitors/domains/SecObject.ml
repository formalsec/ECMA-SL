exception Exists of string

type t = (Field.t, (SecLevel.t * SecLevel.t)) Hashtbl.t

let create () : t = Hashtbl.create 511

let get (obj : t) (f : Field.t) : (SecLevel.t * SecLevel.t) =
  let res = Hashtbl.find_opt obj f in
  match res with
  | None -> raise (Exists "Entry not found")
  | Some (value_lvl,exist_lvl) ->  (value_lvl,exist_lvl)

let set (obj : t) (f : Field.t) (value_lvl : SecLevel.t) (exist_lvl : SecLevel.t) : unit = Hashtbl.add obj f (value_lvl,exist_lvl)

let delete (obj : t) (f : Field.t) : unit = Hashtbl.remove obj f

let str (obj : t) : string =
  (Hashtbl.fold (fun k v ac ->
       match v with
       | (value_lvl,exist_lvl) ->
         (if ac <> "{ " then ac ^ ", " else ac) ^ (Printf.sprintf "%s: %s,%s" (Field.str k) (SecLevel.str value_lvl)(SecLevel.str exist_lvl))) obj "{ ") ^ " }"
