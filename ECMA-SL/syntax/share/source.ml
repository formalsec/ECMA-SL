type pos =
  { file : string
  ; line : int
  ; column : int
  }

type region =
  { left : pos
  ; right : pos
  }

type 'a phrase =
  { it : 'a
  ; at : region
  }

let ( @> ) (x : 'a) (region : region) = { it = x; at = region }
let no_pos = { file = ""; line = 0; column = 0 }
let no_region = { left = no_pos; right = no_pos }

let string_of_pos (pos : pos) : string =
  if pos.line = -1 then Printf.sprintf "0x%x" pos.column
  else string_of_int pos.line ^ "." ^ string_of_int pos.column

let string_of_region (r : region) : string =
  r.left.file
  ^ ":"
  ^ string_of_pos r.left
  ^ if r.right = r.left then "" else "-" ^ string_of_pos r.right

(* Source code lines loaded *)

module Code = struct
  type file = string list
  type t = (string, file) Hashtbl.t

  let code : t = Hashtbl.create !Config.default_hashtbl_sz

  let load (file : string) (data : string) : unit =
    Hashtbl.replace code file (String.split_on_char '\n' data)

  let line (file : string) (loc : int) : string =
    let line' file = List.nth_opt file loc in
    Option.bind (Hashtbl.find_opt code file) line' |> Option.value ~default:""
end
