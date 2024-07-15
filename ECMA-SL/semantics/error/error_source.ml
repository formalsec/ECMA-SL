open EslBase
open EslSyntax

type t =
  | Region of Source.at
  | Index of int

let none () = Region Source.none
let from (el : 'a Source.t) : t = Region el.at
let at (at : Source.at) : t = Region at
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

  let pp_location (ppf : Fmt.t) (at : Source.at) : unit =
    let open Source in
    let pp_locdata ppf at =
      Fmt.fmt ppf "File %S, line %d, characters %d-%d" at.file at.lpos.line
        at.lpos.col at.rpos.col
    in
    Font.pp_err [ Font.Italic; Font.Faint ] pp_locdata ppf at

  let pp_indent (ppf : Fmt.t) (lineno : int) : unit =
    let lineno_sz = String.length (string_of_int lineno) in
    Fmt.pp_str ppf (String.make (lineno_sz + 5) ' ')

  let pp_highlight (ppf : Fmt.t) ((code, left, right) : string * int * int) :
    unit =
    let base = Str.(global_replace (regexp "[^ \t\r\n]") " " code) in
    Fmt.fmt ppf "%s%a" (String.sub base 0 left)
      (Font.pp_text_err ErrorType.font)
      (String.make (right - left) '^')

  let pp_at (ppf : Fmt.t) (at : Source.at) : unit =
    (* FIXME: Improve this for multiple-lines *)
    let file = at.file in
    let line = at.lpos.line in
    let left = at.lpos.col in
    let right = at.rpos.col in
    let (start, code) = format_code (Code_utils.line file line) in
    let (left', right') = (left - start, right - start) in
    Fmt.fmt ppf "\n%a\n%d |   %s\n%a%a" pp_location at line code pp_indent line
      pp_highlight (code, left', right')

  let pp (ppf : Fmt.t) (src : t) : unit =
    match src with
    | Region at when at = Source.none -> ()
    | Region at -> pp_at ppf at
    | _ -> ()

  let str (src : t) : string = Fmt.str "%a" pp src
end
