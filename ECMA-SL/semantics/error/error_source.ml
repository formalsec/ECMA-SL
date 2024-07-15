open EslBase
open EslSyntax.Source

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
    try List.nth lst (i - 1) with _ -> Log.fail "expecting in-bound index" )
  | _ -> Log.fail "expecting index token"

module ErrSrcFmt (ErrorType : Error_type.ERROR_TYPE) = struct
  let format_code (code : string) : int * string =
    let start = Str.(search_forward (regexp "[^ \t\r\n]") code 0) in
    (start, String.sub code start (String.length code - start))

  let pp_location (ppf : Fmt.t) (region : region) : unit =
    let pp_locdata ppf region =
      Fmt.fmt ppf "File %S, line %d, characters %d-%d" region.file
        region.left.line region.left.column region.right.column
    in
    Font.pp_err [ Font.Italic; Font.Faint ] pp_locdata ppf region

  let pp_indent (ppf : Fmt.t) (lineno : int) : unit =
    let lineno_sz = String.length (string_of_int lineno) in
    Fmt.pp_str ppf (String.make (lineno_sz + 5) ' ')

  let pp_highlight (ppf : Fmt.t) ((code, left, right) : string * int * int) :
    unit =
    let base = Str.(global_replace (regexp "[^ \t\r\n]") " " code) in
    Fmt.fmt ppf "%s%a" (String.sub base 0 left)
      (Font.pp_text_err ErrorType.font)
      (String.make (right - left) '^')

  let pp_region (ppf : Fmt.t) (region : region) : unit =
    (* FIXME: Improve this for multiple-lines *)
    let file = region.file in
    let line = region.left.line in
    let left = region.left.column in
    let right = region.right.column in
    let (start, code) = format_code (Code_utils.line file line) in
    let (left', right') = (left - start, right - start) in
    Fmt.fmt ppf "\n%a\n%d |   %s\n%a%a" pp_location region line code pp_indent
      line pp_highlight (code, left', right')

  let pp (ppf : Fmt.t) (src : t) : unit =
    match src with
    | Region region when region = no_region -> ()
    | Region region -> pp_region ppf region
    | _ -> ()

  let str (src : t) : string = Fmt.str "%a" pp src
end
