open Yojson.Basic

let read_primitive_type (p_type : Yojson.Basic.t) : Val.t =
  match p_type with
  | `Null     -> Val.Symbol "undefined"
  | `Bool b   -> Bool b
  | `Int i    -> Int i
  | `Float f  -> Flt f
  | `String s -> Str s
  | _         -> invalid_arg "Expecting a primitive type or Null"

let read_json_obj (json_obj : Yojson.Basic.t) : (Field.t * Val.t) list =
  match json_obj with
  | `Assoc fvs -> List.map (fun ((field : string), (value : Yojson.Basic.t)) -> let v = read_primitive_type value in (field, v)) fvs
  | _          -> invalid_arg "The JSON data must start with an object"

let json_to_heap (data : Yojson.Basic.t) : (string * (Field.t * Val.t) list) list =
  match data with
  | `Assoc objs -> List.map (fun ((loc : string), (json_obj : Yojson.Basic.t)) -> let obj = read_json_obj json_obj in (loc, obj)) objs
  | _           -> invalid_arg "The JSON data must start with an object"

let parse (js_file : string) : (string * (Field.t * Val.t) list) list =
  let data = Yojson.Basic.from_file js_file in
  json_to_heap data
