type 'a obj = 'a Object.t
type 'a t = (Loc.t, 'a obj) Hashtbl.t

let create () : 'a t = Hashtbl.create !Flags.default_hashtbl_sz

let clone (heap : 'a t) : 'a t =
  let heap' = create () in
  Hashtbl.iter (fun l o -> Hashtbl.replace heap' l (Object.clone o)) heap;
  heap'

let insert (heap : 'a t) (obj : 'a obj) : Loc.t =
  let loc = Loc.newloc () in
  Hashtbl.replace heap loc obj;
  loc

let remove (heap : 'a t) (loc : Loc.t) : unit = Hashtbl.remove heap loc

let update (heap : 'a t) (loc : Loc.t) (obj : 'a obj) : unit =
  Hashtbl.replace heap loc obj

let get (heap : 'a t) (loc : Loc.t) : 'a obj option = Hashtbl.find_opt heap loc

let get_field (heap : 'a t) (loc : Loc.t) (field : Field.t) : 'a option =
  let obj = get heap loc in
  let v = match obj with None -> None | Some o -> Object.get o field in
  v

let set_field (heap : 'a t) (loc : Loc.t) (field : Field.t) (v : 'a) : unit =
  let obj = get heap loc in
  match obj with None -> () | Some o -> Object.set o field v

let delete_field (heap : 'a t) (loc : Loc.t) (field : Field.t) : unit =
  let obj = get heap loc in
  match obj with None -> () | Some o -> Object.delete o field

let to_string (h : 'a t) (pp : 'a -> string) : string =
  "{ "
  ^ String.concat ", "
      (Hashtbl.fold
         (fun n v acc ->
           Printf.sprintf "%s: %s" (Loc.str n) (Object.to_string v pp) :: acc)
         h [])
  ^ " }"

let to_string_with_glob (h : 'a t) (pp : 'a -> string) : string =
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
      Printf.sprintf "{ \"heap\": %s, \"global\": %s }" (to_string h pp)
        (Val.str l)
  | None ->
      raise
        (Failure
           "Couldn't find the Object that contains only one property, named \
            \"global\".")
