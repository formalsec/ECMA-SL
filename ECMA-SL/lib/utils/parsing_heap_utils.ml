open Val

let rec read_primitive_type (p_type : Yojson.Basic.t) : Val.t =
  match p_type with
  | `Null -> Null
  | `Bool b -> Bool b
  | `Int i -> Flt (float_of_int i)
  | `Float f -> Flt f
  | `List l -> List (List.map read_primitive_type l)
  | `String s -> (
      let loc_prefix = "$loc_global_" in
      let len_l = String.length loc_prefix in
      let len_s = String.length s in
      let len = if len_l < len_s then len_l else len_s in
      let sub_str = String.sub s 0 len in
      if sub_str = loc_prefix then Loc s
      else
        match s with
        | "'empty" -> Symbol "empty"
        | "'undefined" -> Symbol "undefined"
        | "'null" -> Symbol "null"
        | "nan" -> Flt (float_of_string "nan")
        | "-inf" -> Flt (float_of_string "-infinity")
        | "inf" -> Flt (float_of_string "infinity")
        | _ -> Str s)
  | _ -> invalid_arg "Expecting a primitive type or Null"

let read_json_obj (json_obj : Yojson.Basic.t) : Val.t Object.t =
  match json_obj with
  | `Assoc fvs ->
      let obj = Object.create () in
      List.iter
        (fun ((field : string), (value : Yojson.Basic.t)) ->
          let v = read_primitive_type value in
          Object.set obj field v)
        fvs;
      obj
  | _ -> invalid_arg "The JSON data must start with an object"

let json_to_heap (heap : Heap.t) (data : Yojson.Basic.t) : unit =
  let update_heap_inplace ((loc, json_obj) : Loc.t * Yojson.Basic.t) : unit =
    let obj = read_json_obj json_obj in
    Heap.update heap loc obj
  in
  match data with
  | `Assoc objs -> List.iter update_heap_inplace objs
  | _ -> invalid_arg "The JSON data must start with an object"

let parse_and_update (heap : Heap.t) (js_file : string) : string =
  let data = Yojson.Basic.from_file js_file in
  let heap_in_file, global_loc =
    match data with
    | `Assoc heap_global ->
        if List.length heap_global <> 2 then
          invalid_arg
            "Unexpected Heap length: expecting a JSON object with two \
             properties."
        else (List.hd heap_global, List.nth heap_global 1)
    | _ -> invalid_arg "Unexpected heap file format. Expecting a JSON object."
  in
  if fst heap_in_file <> "heap" then
    invalid_arg "Heap wasn't found in heap file.";
  if fst global_loc <> "global" then
    invalid_arg "Location of the global object wasn't found in heap file.";
  ignore (json_to_heap heap (snd heap_in_file));
  match snd global_loc with
  | `String l -> l
  | _ -> raise (Failure "Error getting location of the global object.")
