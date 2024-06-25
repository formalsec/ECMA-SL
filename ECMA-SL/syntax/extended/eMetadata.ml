open EslBase
module Value = Smtml.Value

module Stmt = struct
  type t =
    { where : string
    ; html : string
    }
end

module Func = struct
  type t =
    { section_number : string
    ; section_name : string option
    ; pre_text : string
    ; post_text : string
    ; meta_vars : (string * string) list
    }

  let section_number (meta : t) : string = meta.section_number
  let pre_text (meta : t) : string = meta.pre_text
  let post_text (meta : t) : string = meta.post_text
  let section_name (meta : t) : string option = meta.section_name
  let meta_vars (meta : t) : (string * string) list = meta.meta_vars

  let build (metadata : Value.t list) (meta_vars : (string * string) list) : t =
    let section_number =
      match List.nth_opt metadata 0 with
      | None -> ""
      | Some (Str section) -> section
      | Some v ->
        Log.fail (* FIXME: Replace by proper syntax error *)
          "Unexpected metadata value '%a' for section number (index 0): \
           expecting a string value"
          Value.pp v
    in
    let pre_text =
      match List.nth_opt metadata 1 with
      | None -> ""
      | Some (Str pre_text) -> pre_text
      | Some v ->
        Log.fail (* FIXME: Replace by proper syntax error *)
          "Unexpected metadata value '%a' for section pre-text (index 1): \
           expecting a string value"
          Value.pp v
    in
    let post_text =
      match List.nth_opt metadata 2 with
      | None -> ""
      | Some (Str post_text) -> post_text
      | Some v ->
        Log.fail (* FIXME: Replace by proper syntax error *)
          "Unexpected metadata value '%a' for section post-text (index 2): \
           expecting a string value"
          Value.pp v
    in
    let section_name =
      match List.nth_opt metadata 3 with
      | None -> Some ""
      | Some (App (`Op "null", [])) -> None
      | Some (Str section_name) -> Some section_name
      | Some v ->
        Log.fail (* FIXME: Replace by proper syntax error *)
          "Unexpected metadata value '%a' for section name (index 3): \
           expecting a string or null value"
          Value.pp v
    in
    { section_number; section_name; pre_text; post_text; meta_vars }
end

module Pat = struct
  type t =
    { production_number : string
    ; production_name : string option
    ; production_text : string
    ; pre_text : string
    ; post_text : string
    ; meta_vars : (string * string) list
    }

  let production_number (meta : t) : string = meta.production_number
  let production_name (meta : t) : string option = meta.production_name
  let production_text (meta : t) : string = meta.production_text
  let pre_text (meta : t) : string = meta.pre_text
  let post_text (meta : t) : string = meta.post_text
  let meta_vars (meta : t) : (string * string) list = meta.meta_vars

  let build (metadata : Value.t list) (meta_vars : (string * string) list) : t =
    let production_number =
      match List.nth_opt metadata 0 with
      | None -> ""
      | Some (Str production_number) -> production_number
      | Some v ->
        Log.fail (* FIXME: Replace by proper syntax error *)
          "Unexpected metadata value '%a' for production number (index 0): \
           expecting a string value"
          Value.pp v
    in

    let pre_text =
      match List.nth_opt metadata 1 with
      | None -> ""
      | Some (Str pre_text) -> pre_text
      | Some v ->
        Log.fail (* FIXME: Replace by proper syntax error *)
          "Unexpected metadata value '%a' for production pre-text (index 1): \
           expecting a string value"
          Value.pp v
    in

    let production_text =
      match List.nth_opt metadata 2 with
      | None -> ""
      | Some (Str production_text) -> production_text
      | Some v ->
        Log.fail (* FIXME: Replace by proper syntax error *)
          "Unexpected metadata value '%a' for production text (index 2): \
           expecting a string value"
          Value.pp v
    in
    let post_text =
      match List.nth_opt metadata 3 with
      | None -> ""
      | Some (Str post_text) -> post_text
      | Some v ->
        Log.fail (* FIXME: Replace by proper syntax error *)
          "Unexpected metadata value '%a' for production post-text (index 3): \
           expecting a string value"
          Value.pp v
    in
    let production_name =
      match List.nth_opt metadata 4 with
      | None -> Some ""
      | Some (App (`Op "null", [])) -> None
      | Some (Str production_name) -> Some production_name
      | Some v ->
        Log.fail (* FIXME: Replace by proper syntax error *)
          "Unexpected metadata value '%a' for production name (index 4): \
           expecting a string or null value"
          Value.pp v
    in
    { production_number
    ; production_name
    ; production_text
    ; pre_text
    ; post_text
    ; meta_vars
    }
end
