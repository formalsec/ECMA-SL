open EslBase
open EslSyntax

type file = string array
type t = (string, file) Hashtbl.t

let code : t = Hashtbl.create !Base.default_hashtbl_sz

let load_file (fname : string) (data : string) : unit =
  let lines = String.split_on_char '\n' data in
  Hashtbl.replace code fname (Array.of_list lines)

let file_sz (file : file) : int = Array.length file [@@inline]

let file (fname : string) : file =
  match Hashtbl.find_opt code fname with
  | Some file -> file
  | None -> Log.fail "expecting loaded file path, but got '%s'" fname

let line (file : file) (lineno : int) : int * string =
  try (lineno, Array.get file (lineno - 1))
  with Invalid_argument _ ->
    Log.fail "expecting line between 1 and %d, but got %d" (file_sz file) lineno

let rec lines (file : file) (start : int) (nlines : int) : (int * string) list =
  if nlines == 0 then []
  else line file start :: lines file (start + 1) (nlines - 1)

let codeblock (at : Source.at) : string list =
  let trim_line line n =
    match (at.lpos.line, at.rpos.line) with
    | (left, right) when left == n && right == n ->
      String.substr ~left:at.lpos.col ~right:at.rpos.col line
    | (l, _) when l == n -> String.substr ~left:at.lpos.col line
    | (_, r) when r == n -> String.substr ~right:at.rpos.col line
    | _ -> line
  in
  let rec trim_lines = function
    | [] -> []
    | (n, line) :: lines' -> trim_line line n :: trim_lines lines'
  in
  let start = at.lpos.line in
  let nlines = at.rpos.line - at.lpos.line + 1 in
  trim_lines (lines (file at.file) start nlines)

let pp (ppf : Format.formatter) (at : Source.at) : unit =
  let sep ppf () = Fmt.pf ppf "@\n" in
  Fmt.(list ~sep string) ppf (codeblock at)

let str (at : Source.at) : string = Fmt.str "%a" pp at
