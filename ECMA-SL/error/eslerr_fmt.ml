module Msgs (ErrType : Eslerr_type.ERR_TYPE) = struct
  let pp_cause (fmt : Fmt.t) (msg : ErrType.t) : unit =
    Fmt.fprintf fmt "\n%a %a"
      (Font.pp_text_err [ ErrType.font (); Font.Faint ])
      "Caused by:" ErrType.pp msg

  let pp (fmt : Fmt.t) (msgs : ErrType.t list) : unit =
    let open Fmt in
    let header = ErrType.header () ^ ":" in
    let font = [ ErrType.font () ] in
    let err_msgs fmt = function
      | [] -> fprintf fmt "%a" (Font.pp_text_err font) "???"
      | main :: causes ->
        fprintf fmt "%a%a" ErrType.pp main (pp_lst "" pp_cause) causes
    in
    fprintf fmt "\n%a %a" (Font.pp_text_err font) header err_msgs msgs

  let str (msgs : ErrType.t list) : string = Fmt.asprintf "%a" pp msgs
end

module Code (ErrType : Eslerr_type.ERR_TYPE) = struct
  module Src = Eslerr_comp.ErrSrc
  open Source

  let format_code (code : string) : int * string =
    let start = Str.(search_forward (regexp "[^ \t\r\n]") code 0) in
    (start, String.sub code start (String.length code - start))

  let pp_location (fmt : Fmt.t) (region : region) : unit =
    let pp_locdata fmt region =
      Fmt.fprintf fmt "File %S, line %d, characters %d-%d" region.file
        region.left.line region.left.column region.right.column
    in
    Font.pp_err [ Font.Italic; Font.Faint ] pp_locdata fmt region

  let pp_indent (fmt : Fmt.t) (lineno : int) : unit =
    let lineno_sz = String.length (string_of_int lineno) in
    Fmt.pp_str fmt (String.make (lineno_sz + 5) ' ')

  let pp_highlight (fmt : Fmt.t) ((code, left, right) : string * int * int) :
    unit =
    let base = Str.(global_replace (regexp "[^ \t\r\n]") " " code) in
    Fmt.fprintf fmt "%s%a" (String.sub base 0 left)
      (Font.pp_text_err [ ErrType.font () ])
      (String.make (right - left) '^')

  let pp_region (fmt : Fmt.t) (region : region) : unit =
    let (file, line, left, right) = region_unfold region in
    let (start, code) = format_code (Source.Code.line file line) in
    let (left', right') = (left - start, right - start) in
    Fmt.fprintf fmt "\n%a\n%d |   %s\n%a%a" pp_location region line code
      pp_indent line pp_highlight (code, left', right')

  let pp (fmt : Fmt.t) (src : Src.t) : unit =
    match src with
    | Region region when region = no_region -> ()
    | Region region -> pp_region fmt region
    | _ -> ()
end

module Custom (ErrType : Eslerr_type.ERR_TYPE) = struct
  module RtTrace = Eslerr_comp.RtTrace

  let pp_trace (fmt : Fmt.t) (trace : RtTrace.t option) : unit =
    match trace with
    | None -> ()
    | Some trace_pp -> Fmt.fprintf fmt "\nRaised at %a" trace_pp ()
end
