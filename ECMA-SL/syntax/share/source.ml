open EslCore

type pos =
  { line : int
  ; column : int
  }

type region =
  { file : string
  ; left : pos
  ; right : pos
  }

let no_pos = { line = -1; column = -1 }
let no_region = { file = ""; left = no_pos; right = no_pos }

let region_unfold (region : region) : string * int * int * int =
  (region.file, region.left.line, region.left.column, region.right.column)

let pp_pos (fmt : Fmt.t) (pos : pos) : unit =
  let open Fmt in
  let pp_pos' fmt pos = if pos = -1 then pp_str fmt "x" else pp_int fmt pos in
  fprintf fmt "%a.%a" pp_pos' pos.line pp_pos' pos.column

let pp_region (fmt : Fmt.t) (region : region) : unit =
  Fmt.fprintf fmt "%S:%a-%a" region.file pp_pos region.left pp_pos region.right

type +'a phrase =
  { it : 'a
  ; at : region
  }

let ( @> ) (x : 'a) (region : region) : 'a phrase = { it = x; at = region }
let pp (fmt : Fmt.t) (x : 'a phrase) = Fmt.fprintf fmt "%a" pp_region x.at
let str (x : 'a phrase) : string = Fmt.asprintf "%a" pp x

module Code = struct
  type file = string list
  type t = (string, file) Hashtbl.t

  let code : t = Hashtbl.create !Config.default_hashtbl_sz

  let load (file : string) (data : string) : unit =
    Hashtbl.replace code file (String.split_on_char '\n' data)

  let line (file : string) (loc : int) : string =
    let line' file = List.nth_opt file (loc - 1) in
    Option.bind (Hashtbl.find_opt code file) line' |> Option.value ~default:""
end
