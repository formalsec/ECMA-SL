open EslCore
open Source

type t =
  | Region of region
  | Index of int

let none () = Region no_region
let at (el : 'a phrase) : t = Region el.at
let region (region : region) : t = Region region
let index (index : int) : t = Index index

let index_to_el (lst : 'a list) (src : t) : 'a =
  match src with
  | Index i -> (
    try List.nth lst (i - 1)
    with _ -> Internal_error.(throw __FUNCTION__ (Expecting "in-bound index")) )
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "index token"))

module ErrSrcFmt (ErrorType : Error_type.ERROR_TYPE) = struct
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
      (Font.pp_text_err ErrorType.font)
      (String.make (right - left) '^')

  let pp_region (fmt : Fmt.t) (region : region) : unit =
    let (file, line, left, right) = region_unfold region in
    let (start, code) = format_code (Code.line file line) in
    let (left', right') = (left - start, right - start) in
    Fmt.fprintf fmt "\n%a\n%d |   %s\n%a%a" pp_location region line code
      pp_indent line pp_highlight (code, left', right')

  let pp (fmt : Fmt.t) (src : t) : unit =
    match src with
    | Region region when region = no_region -> ()
    | Region region -> pp_region fmt region
    | _ -> ()

  let str (src : t) : string = Fmt.asprintf "%a" pp src
end
