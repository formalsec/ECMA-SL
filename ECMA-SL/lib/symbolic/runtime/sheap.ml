module Symbolic_heap = Map.Make (String)

type object_t = Sval.t Sobject.t
type t = object_t Symbolic_heap.t

let create () : t = Symbolic_heap.empty
let add (h : t) (l : Loc.t) (o : object_t) : t = Symbolic_heap.add l o h
let remove (h : t) (l : Loc.t) : t = Symbolic_heap.remove l h

let update (h : t) (l : Loc.t) (o : object_t) : t =
  Symbolic_heap.update l (fun _ -> Some o) h

let find_opt (h : t) (l : Loc.t) : object_t option = Symbolic_heap.find_opt l h

let get_field (h : t) (l : Loc.t) (f : Field.t) : Sval.t option =
  let obj = find_opt h l in
  let v = match obj with None -> None | Some o -> Sobject.find_opt o f in
  v

let set_field (h : t) (l : Loc.t) (f : Field.t) (v : Sval.t) : t =
  (* Deletes binding l if (find_opt l h) is None *)
  Symbolic_heap.update l (fun o' -> Sobject.add_opt o' f v) h

let delete_field (h : t) (l : Loc.t) (f : Field.t) : t =
  (* Deletes binding l if (find_opt l h) is None *)
  Symbolic_heap.update l (fun o' -> Sobject.remove_opt o' f) h

let object_to_string (o : object_t) : string =
  let str_obj =
    Sobject.Symbolic_object.fold
      (fun n v ac ->
        if ac <> "{ " then ac ^ ", "
        else
          ac
          ^ Printf.sprintf "%s: %s" (Field.str n)
              (Sval.str ~flt_with_dot:false v))
      o "{ "
  in
  str_obj ^ " }"

let object_to_json (o : object_t) : string =
  let str_obj =
    Sobject.Symbolic_object.fold
      (fun n v ac ->
        if ac <> "{ " then ac ^ ", "
        else
          ac
          ^ Printf.sprintf "\"%s\": %s" (Field.str n)
              (Sval.str ~flt_with_dot:false v))
      o "{ "
  in
  str_obj ^ " }"

let to_string (h : t) : string =
  "{ "
  ^ String.concat ", "
      (Symbolic_heap.fold
         (fun n v acc ->
           Printf.sprintf "%s: %s" (Loc.str n) (object_to_string v) :: acc)
         h [])
  ^ " }"

let to_string_with_glob (h : t) : string =
  let global =
    Symbolic_heap.fold
      (fun _ obj acc ->
        match acc with
        | Some _ -> acc
        (* Keep this in sync with Compiler.ml function *)
        (* "compile_gvar" and "compile_glob_assign" *)
        | None -> Sobject.find_opt obj Common.global_var_compiled)
      h None
  in
  match global with
  | Some l ->
      Printf.sprintf "{ \"heap\": %s, \"global\": %s }" (to_string h)
        (Sval.str l)
  | None ->
      raise
        (Failure
           "Couldn't find the Object that contains only one property, named \
            \"global\".")
