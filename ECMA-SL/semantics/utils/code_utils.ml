open EslBase
open EslSyntax

type file = string list
type t = (string, file) Hashtbl.t

let code : t = Hashtbl.create !Base.default_hashtbl_sz

let load (file : string) (data : string) : unit =
  Hashtbl.replace code file (String.split_on_char '\n' data)

let get_file (path : string) : file option = Hashtbl.find_opt code path

let get_file_size (file : file option) : int =
  Option.map List.length file |> Option.value ~default:(-1)

let get_line (file : file option) (loc : int) : string =
  let line' file = List.nth_opt file (loc - 1) in
  Option.bind file line' |> Option.value ~default:""

let rec get_lines (file : file option) (start : int) (nlines : int) :
  (int * string) list =
  if nlines == 0 then []
  else (start, get_line file start) :: get_lines file (start + 1) (nlines - 1)

let line (fname : string) (loc : int) : string =
  get_line (Hashtbl.find_opt code fname) loc

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
  trim_lines (get_lines (Hashtbl.find_opt code at.file) start nlines)

let pp (ppf : Fmt.t) (at : Source.at) : unit =
  Fmt.(pp_lst !>"\n" pp_str) ppf (codeblock at)

let str (at : Source.at) : string = Fmt.str "%a" pp at
