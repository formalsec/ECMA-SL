open Val

type t = {
  section_number : string;
  pre : string;
  post: string;
  section_name : string option;
  meta_params : (string * string) list; (** parameter name and its alternative text that is used in the standard HTML generator. *)
}


let get_pre (meta : t) : string = meta.pre
let get_post (meta : t) : string = meta.post
let get_section_number (meta : t) : string = meta.section_number
let get_section_name (meta : t) : string option = meta.section_name
let get_meta_params (meta : t) : (string * string) list = meta.meta_params


let build_func_metadata (metadata : Val.t list) (params_alternatives : (string * string) list) : t =
  let section_number =
    match List.nth_opt metadata 0 with
    | None         -> ""
    | Some (Str s) -> s
    | _            -> invalid_arg "Unexpected metadata value type for section number (list index 0): expecting a String value" in
  let pre =
    match List.nth_opt metadata 1 with
    | None         -> ""
    | Some (Str s) -> s
    | _            -> invalid_arg "Unexpected metadata value type for section pre-text (list index 1): expecting a String value" in
  let post =
    match List.nth_opt metadata 2 with
    | None         -> ""
    | Some (Str s) -> s
    | _            -> invalid_arg "Unexpected metadata value type for section post-text (list index 2): expecting a String value" in
  let section_name =
    match List.nth_opt metadata 3 with
    | None         -> Some ""
    | Some (Null)  -> None
    | Some (Str s) -> Some s
    | _            -> invalid_arg "Unexpected metadata value type for section name (list index 3): expecting a String value or Null" in
  {
    section_number;
    pre;
    post;
    section_name;
    meta_params=params_alternatives;
  }


let two_digits (value : string) : string =
  match String.length value with
  | 0 -> "00"
  | 1 -> "0" ^ value
  | 2 -> value
  | _ -> invalid_arg "Unexpected string length"


let compare_sec_names (meta1 : t) (meta2 : t) : int =
  let meta1_sec_name = get_section_number meta1 in
  let meta2_sec_name = get_section_number meta2 in
  let meta1_sec_name_list = String.split_on_char '.' meta1_sec_name in
  let meta2_sec_name_list = String.split_on_char '.' meta2_sec_name in
  let meta1_sec_name_list' = List.map two_digits meta1_sec_name_list in
  let meta2_sec_name_list' = List.map two_digits meta2_sec_name_list in
  let meta1_sec_name' = String.concat "." meta1_sec_name_list' in
  let meta2_sec_name' = String.concat "." meta2_sec_name_list' in
  String.compare meta1_sec_name' meta2_sec_name'

