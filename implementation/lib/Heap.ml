type t = (Loc.t, Object.t) Hashtbl.t

let create () : t = Hashtbl.create 511

let insert ?(loc=Loc.newloc()) (heap : t) (obj : Object.t) : Loc.t = Hashtbl.add heap loc obj; loc

let remove (heap : t) (loc : Loc.t) : unit = Hashtbl.remove heap loc

let update (heap : t) (loc : Loc.t) (obj : Object.t) : unit = Hashtbl.replace heap loc obj

let get (heap : t) (loc : Loc.t) : Object.t option = Hashtbl.find_opt heap loc

let get_field (heap : t) (loc : Loc.t) (field : Field.t) : Val.t option =
  let obj = get heap loc in
  let v = match obj with
    | None   -> None
    | Some o -> Object.get o field in
  v

let set_field (heap : t) (loc : Loc.t) (field : Field.t) (value : Val.t) : unit =
  let obj = get heap loc in
  match obj with
  | None   -> ()
  | Some o -> Object.set o field value

let delete_field (heap : t) (loc : Loc.t) (field : Field.t) : unit =
  let obj = get heap loc in
  match obj with
  | None   -> ()
  | Some o -> Object.delete o field

let str (heap : t) : string = (Hashtbl.fold (fun n v ac -> (if ac <> "{ " then ac ^ ", " else ac) ^ (Printf.sprintf "%s: %s" (Loc.str n) (Object.str v))) heap "{ ") ^ " }"


let from_list_to_hashtbl (h_list : (string * (Field.t * Val.t) list) list) : t =
  let heap = create () in
  List.iter (fun (o : (string * (Field.t * Val.t) list)) ->
      let lo = fst o and fvs = snd o in
      let obj = Object.create() in
      List.iter (fun (fv : Field.t * Val.t) -> Object.set obj (fst fv) (snd fv)) fvs;
      ignore (insert ~loc:lo heap obj)
    ) h_list; heap

let from_json_file (filename : string) : t =
  let h_list = Parse_Heap.parse filename in
  from_list_to_hashtbl h_list
