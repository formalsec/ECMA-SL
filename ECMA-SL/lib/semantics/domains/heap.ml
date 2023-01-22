type object_t = Val.t Object.t
type t = (Loc.t, object_t) Hashtbl.t

let create () : t = Hashtbl.create Common.default_hashtable_size

let insert (heap : t) (obj : object_t) : Loc.t =
  let loc = Loc.newloc () in
  Hashtbl.replace heap loc obj;
  loc

let remove (heap : t) (loc : Loc.t) : unit = Hashtbl.remove heap loc

let update (heap : t) (loc : Loc.t) (obj : object_t) : unit =
  Hashtbl.replace heap loc obj

let get (heap : t) (loc : Loc.t) : object_t option = Hashtbl.find_opt heap loc

let get_field (heap : t) (loc : Loc.t) (field : Field.t) : Val.t option =
  let obj = get heap loc in
  let v = match obj with None -> None | Some o -> Object.get o field in
  v

let set_field (heap : t) (loc : Loc.t) (field : Field.t) (value : Val.t) : unit
    =
  let obj = get heap loc in
  match obj with None -> () | Some o -> Object.set o field value

let delete_field (heap : t) (loc : Loc.t) (field : Field.t) : unit =
  let obj = get heap loc in
  match obj with None -> () | Some o -> Object.delete o field

let object_to_string (o : object_t) : string =
  let str_obj =
    Hashtbl.fold
      (fun n v ac ->
        if ac <> "{ " then ac ^ ", "
        else
          ac
          ^ Printf.sprintf "%s: %s" (Field.str n)
              (Val.str ~flt_with_dot:false v))
      o "{ "
  in
  str_obj ^ " }"

let object_to_json (o : object_t) : string =
  let str_obj =
    Hashtbl.fold
      (fun n v ac ->
        if ac <> "{ " then ac ^ ", "
        else
          ac
          ^ Printf.sprintf "\"%s\": %s" (Field.str n)
              (Val.str ~flt_with_dot:false v))
      o "{ "
  in
  str_obj ^ " }"

let to_string (h : t) : string =
  "{ "
  ^ String.concat ", "
      (Hashtbl.fold
         (fun n v acc ->
           Printf.sprintf "%s: %s" (Loc.str n) (object_to_string v) :: acc)
         h [])
  ^ " }"

let to_string_with_glob (h : t) : string =
  let glob =
    Hashtbl.fold
      (fun _ obj acc ->
        match acc with
        | Some _ -> acc
        (* Keep this in sync with Compiler.ml function *)
        (* "compile_gvar" and "compile_glob_assign" *)
        | None -> Object.get obj Common.global_var_compiled)
      h None
  in
  match glob with
  | Some l ->
      Printf.sprintf "{ \"heap\": %s, \"global\": %s }" (to_string h)
        (Val.str l)
  | None ->
      raise
        (Failure
           "Couldn't find the Object that contains only one property, named \
            \"global\".")
