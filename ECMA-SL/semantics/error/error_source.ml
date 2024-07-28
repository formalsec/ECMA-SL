open EslBase
open EslSyntax

type t = Source.at

let none () : t = Source.none [@@inline]
let at (el : 'a Source.t) : t = el.at [@@inline]

module ErrSrcFmt (ErrorType : Error_type.ERROR_TYPE) = struct
  type location =
    { file : string
    ; line : int
    ; lpos : int
    ; rpos : int
    }

  let location (at : Source.at) : location =
    let (file, line, lpos) = (at.file, at.lpos.line, at.lpos.col) in
    let rpos = if at.lpos.line == at.rpos.line then at.rpos.col else -1 in
    { file; line; lpos; rpos }

  let format_code (line : string) : int * string =
    let start = Str.search_forward (Str.regexp "[^ \t\r\n]") line 0 in
    let line = String.sub line start (String.length line - start) in
    (start, line)

  let pp_loc : Fmt.t -> location -> unit =
    Font.pp_err [ Font.Italic; Font.Faint ] @@ fun ppf loc ->
    Fmt.fmt ppf "File %S, line %d, characters %d-%d" loc.file loc.line loc.lpos
      loc.rpos

  let pp_indent (ppf : Fmt.t) (lineno : int) : unit =
    let lineno_sz = String.length (string_of_int lineno) in
    Fmt.pp_str ppf (String.make (lineno_sz + 5) ' ')

  let pp_hglt (ppf : Fmt.t) ((code, lpos, rpos) : string * int * int) : unit =
    let pp_font = Font.pp_text_err ErrorType.font in
    let code' = Str.global_replace (Str.regexp "[^ \t\r\n]") " " code in
    Fmt.fmt ppf "%s%a" (String.sub code' 0 lpos) pp_font
      (String.make (rpos - lpos) '^')

  let pp (ppf : Fmt.t) (at : t) : unit =
    if at != Source.none then
      let loc = location at in
      let (_, line) = Code_utils.(line (file loc.file) loc.line) in
      let rpos' = if loc.rpos != -1 then loc.rpos else String.length line in
      let (start, code) = format_code line in
      let (lpos, rpos) = (loc.lpos - start, rpos' - start) in
      Fmt.fmt ppf "@\n%a@\n%d |   %s@\n%a%a" pp_loc loc loc.line code pp_indent
        loc.line pp_hglt (code, lpos, rpos)

  let str (src : t) : string = Fmt.str "%a" pp src [@@inline]
end
