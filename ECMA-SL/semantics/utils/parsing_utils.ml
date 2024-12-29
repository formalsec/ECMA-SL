open EslBase
open EslSyntax

let load_file ?(file : string option) (path : string) : string =
  let file' = Option.value ~default:path file in
  let data = Io.read_file path in
  Code_utils.load_file file' data;
  data

let init_lexbuf (file : string) (str : string) =
  let lexbuf = Lexing.from_string str in
  { lexbuf with lex_curr_p = { lexbuf.lex_curr_p with pos_fname = file } }

let pos (position : Lexing.position) : Source.pos =
  { line = position.Lexing.pos_lnum
  ; col = position.Lexing.pos_cnum - position.Lexing.pos_bol
  }

let at ((startpos, endpos) : Lexing.position * Lexing.position) : Source.at =
  { file = startpos.Lexing.pos_fname
  ; lpos = pos startpos
  ; rpos = pos endpos
  ; real = true
  }

let print_position (outx : Format.formatter) (lexbuf : Lexing.lexbuf) : unit =
  let pos = lexbuf.lex_curr_p in
  Log.stdout "Line number: %d. File: %s@." pos.pos_lnum pos.pos_fname;
  Fmt.pf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_loc =
  let re = Str.regexp {|\$loc_([0-9]+)|} in
  fun (x : string) : Loc.t ->
    match Str.string_match re x 0 with
    | true -> int_of_string (Str.matched_group 1 x)
    | false -> Log.fail "parse_loc: unable to parse location: '%s'" x
