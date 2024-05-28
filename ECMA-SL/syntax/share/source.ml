open EslBase

type pos =
  { line : int
  ; column : int
  }

type region =
  { file : string
  ; left : pos
  ; right : pos
  ; real : bool
  }

let no_pos = { line = -1; column = -1 }
let no_region = { file = ""; left = no_pos; right = no_pos; real = false }

let region_unfold (region : region) : string * int * int * int =
  (region.file, region.left.line, region.left.column, region.right.column)

let pp_pos (ppf : Fmt.t) (pos : pos) : unit =
  let open Fmt in
  let pp_pos' ppf pos = if pos = -1 then pp_str ppf "x" else pp_int ppf pos in
  format ppf "%a.%a" pp_pos' pos.line pp_pos' pos.column

let pp_region (ppf : Fmt.t) (region : region) : unit =
  Fmt.format ppf "%S:%a-%a" region.file pp_pos region.left pp_pos region.right

type +'a phrase =
  { it : 'a
  ; at : region
  }

let ( @> ) (x : 'a) (region : region) : 'a phrase = { it = x; at = region }

let ( @?> ) (x : 'a) (region : region) : 'a phrase =
  { it = x; at = { region with real = false } }

let map (f : 'a -> 'b) (x : 'a phrase) : 'b phrase = { x with it = f x.it }
let pp (ppf : Fmt.t) (x : 'a phrase) = Fmt.format ppf "%a" pp_region x.at
let str (x : 'a phrase) : string = Fmt.str "%a" pp x

module Code = struct
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

  let codeblock (at : region) : string list =
    let trim_line line n =
      match (at.left.line, at.right.line) with
      | (left, right) when left == n && right == n ->
        String.substr ~left:at.left.column ~right:at.right.column line
      | (l, _) when l == n -> String.substr ~left:at.left.column line
      | (_, r) when r == n -> String.substr ~right:at.right.column line
      | _ -> line
    in
    let rec trim_lines = function
      | [] -> []
      | (n, line) :: lines' -> trim_line line n :: trim_lines lines'
    in
    let start = at.left.line in
    let nlines = at.right.line - at.left.line + 1 in
    trim_lines (get_lines (Hashtbl.find_opt code at.file) start nlines)

  let pp (ppf : Fmt.t) (at : region) : unit =
    Fmt.(pp_lst !>"\n" pp_str) ppf (codeblock at)

  let str (at : region) : string = Fmt.str "%a" pp at
end
