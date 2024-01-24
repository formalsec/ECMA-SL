module Token = Eslerr_token

module type ErrTypeFmt = sig
  type msg

  val font : unit -> Font.t
  val header : unit -> string
  val pp : Fmt.t -> msg -> unit
end

module Msgs (ErrTypeFmt : ErrTypeFmt) = struct
  let pp_cause (fmt : Fmt.t) (msg : ErrTypeFmt.msg) : unit =
    Fmt.fprintf fmt "\n%a %a"
      (Font.pp_text_err [ ErrTypeFmt.font (); Font.Faint ])
      "Caused by:" ErrTypeFmt.pp msg

  let pp (fmt : Fmt.t) (msgs : ErrTypeFmt.msg list) : unit =
    let open Fmt in
    let header = ErrTypeFmt.header () ^ ":" in
    let font = [ ErrTypeFmt.font () ] in
    let err_msgs fmt = function
      | [] -> fprintf fmt "%a" (Font.pp_text_err font) "???"
      | main :: causes ->
        fprintf fmt "%a%a" ErrTypeFmt.pp main (pp_lst "" pp_cause) causes
    in
    fprintf fmt "\n%a %a" (Font.pp_text_err font) header err_msgs msgs

  let str (msgs : ErrTypeFmt.msg list) : string = Fmt.asprintf "%a" pp msgs
end

module Code (ErrTypeFmt : ErrTypeFmt) = struct
  type locdata =
    { region : Source.region
    ; file : string
    ; line : int
    ; left : int
    ; right : int
    }

  type srcdata =
    { code : string
    ; hgl : string
    ; matched : bool
    ; locdata : locdata
    }

  let srcdata_init (loc : Token.t) : srcdata =
    let region = Token.region loc in
    { code = ""
    ; hgl = ""
    ; matched = false
    ; locdata =
        { region
        ; file = region.file
        ; line = region.left.line
        ; left = region.left.column
        ; right = region.right.column
        }
    }

  let srcdata_process_empty (srcdata : srcdata) (loc : Token.t) : srcdata =
    let loc_sz = Font.clean srcdata.code |> String.length in
    let locdata = srcdata.locdata in
    let hgl = String.make loc_sz '^' in
    let left = (Token.region loc).left.column in
    let right = left + loc_sz in
    { srcdata with hgl; locdata = { locdata with left; right } }

  let rec srcdata_process_src (srcdata : srcdata) (src : Token.t) (loc : Token.t)
    : srcdata =
    let write_tkn hgl_chr =
      let (tkn_str, tkn_sz) = Token.str_size loc in
      let code = srcdata.code ^ tkn_str in
      let hgl = srcdata.hgl ^ String.make tkn_sz hgl_chr in
      ({ srcdata with code; hgl }, tkn_sz)
    in
    if srcdata.matched then fst (write_tkn ' ')
    else if Token.cmp loc src then
      let (srcdata', tkn_sz) = write_tkn '^' in
      let matched = true in
      let right = srcdata'.locdata.left + tkn_sz - 1 in
      { srcdata' with matched; locdata = { srcdata'.locdata with right } }
    else if Token.is_splitable loc then
      let split_fun srcdata' loc' = srcdata_process_src srcdata' src loc' in
      List.fold_left split_fun srcdata (Token.split loc)
    else
      let (srcdata', tkn_sz) = write_tkn ' ' in
      let left = srcdata.locdata.left + tkn_sz in
      { srcdata' with locdata = { srcdata'.locdata with left } }

  let srcdata_process (src : Token.t) (loc : Token.t) : srcdata =
    let srcdata = srcdata_init loc in
    let srcdata' = srcdata_process_src srcdata src loc in
    if not srcdata'.matched then srcdata_process_empty srcdata' loc
    else srcdata'

  let pp_location ?(colon : bool = true) (fmt : Fmt.t) (locdata : locdata) :
    unit =
    let open Fmt in
    let colon_str = if colon then ":" else "" in
    let locfont = [ Font.Italic; Font.Faint ] in
    let pp_locdata fmt locdata =
      fprintf fmt "\nFile %S, line %d, characters %d-%d%s" locdata.file
        locdata.line locdata.left locdata.right colon_str
    in
    fprintf fmt "%a" (Font.pp_err locfont pp_locdata) locdata

  let pp_code (fmt : Fmt.t) (srcdata : srcdata) : unit =
    let open Fmt in
    let code_header fmt () =
      if srcdata.locdata.region = Source.no_region then fprintf fmt "      "
      else fprintf fmt "%d |  " srcdata.locdata.line
    in
    fprintf fmt "\n%a%s" code_header () srcdata.code

  let pp_hgl (fmt : Fmt.t) (srcdata : srcdata) : unit =
    let lineno_sz = String.length (string_of_int srcdata.locdata.line) in
    let hgl_indent = String.make (lineno_sz + 4) ' ' in
    let hgl_font = [ ErrTypeFmt.font () ] in
    Fmt.fprintf fmt "\n%s%a" hgl_indent (Font.pp_text_err hgl_font) srcdata.hgl

  let pp (fmt : Fmt.t) ((loc, src) : Token.t * Token.t) : unit =
    match loc with
    | NoTkn -> ()
    | _ ->
      let srcdata = srcdata_process src loc in
      Fmt.fprintf fmt "%a%a%a" (pp_location ~colon:true) srcdata.locdata pp_code
        srcdata pp_hgl srcdata
end

module Custom (ErrTypeFmt : ErrTypeFmt) = struct
  let pp_trace (fmt : Fmt.t) (trace : (Fmt.t -> unit -> unit) option) : unit =
    match trace with
    | None -> ()
    | Some trace_pp -> Fmt.fprintf fmt "\nRaised at %a" trace_pp ()
end
