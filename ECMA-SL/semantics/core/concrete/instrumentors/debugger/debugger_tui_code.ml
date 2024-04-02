open Debugger_tui_helper
open EslBase
open EslSyntax

type t =
  { ui : Win.t Frame.t
  ; at : Source.region
  }

let create (acs : Acs.acs) (termwin : Win.t) : t =
  let (y, x) = (0, 0) in
  let yz = proportional_sz termwin.yz 5 3 in
  let xz = proportional_sz termwin.xz 3 2 in
  let win = Win.mk termwin 1 1 (yz - 2) (xz - 2) in
  let ui = Frame.mk acs termwin y x yz xz (Win.element win) in
  { ui; at = Source.no_region }

let data (code : t) (s : Stmt.t) : t = { code with at = s.at }

let draw_static (code : t) : unit =
  Frame.draw code.ui;
  Frame.refresh code.ui

let render_title (wframe : Win.t) : unit =
  let color_id = 1 in
  !!(init_pair color_id Color.cyan Color.white);
  wattr_set wframe.w WA.(combine [ dim; standout ]) color_id;
  draw_rectangle wframe.w 0 0 4 15 ' ';
  !!(mvwaddstr wframe.w 1 4 "ECMA-SL");
  !!(mvwaddstr wframe.w 2 3 "Debugging")

let render_location_data (win : Win.t) (at : Source.region) : unit =
  let lineno_sz = String.length (string_of_int at.left.line) in
  let (file', trunc) = String.truncate (win.xz - 19 - lineno_sz) at.file in
  let file'' = if trunc then file' ^ "..." else file' in
  let loc = Fmt.sprintf "File %S, line %d" file'' at.left.line in
  !!(mvwaddstr win.w (win.yz - 1) 1 loc)

let render_location (win : Win.t) (at : Source.region) : unit =
  let color_id = 2 in
  !!(init_pair color_id Color.white Color.white);
  wattr_set win.w WA.(combine [ dim; standout ]) color_id;
  draw_rectangle win.w (win.yz - 1) 0 1 win.xz ' ';
  render_location_data win at

let codeblock (nlines : int) (at : Source.region) : (int * string) list * int =
  let file = Source.Code.get_file at.file in
  let file_sz = Source.Code.get_file_size file in
  let line = at.left.line in
  let prev_nlines = proportional_sz nlines 3 1 in
  let codeblock_start = max (line - prev_nlines) 1 in
  let codeblock_sz = min nlines (file_sz - codeblock_start + 1) in
  let codeblock = Source.Code.get_lines file codeblock_start codeblock_sz in
  let last_line = codeblock_start + codeblock_sz in
  (codeblock, last_line)

let render_codeline (win : Win.t) (lineno_sz : int) (i : int)
  ((lineno, line, color_id) : int * string * int) : unit =
  let lineno' = Fmt.sprintf "%*d |" (15 + lineno_sz) lineno in
  let line' = Str.(global_replace (regexp "\t") "  " line) in
  let (line'', trunc) = String.truncate (win.xz - 19 - lineno_sz) line' in
  wattr_set win.w WA.color color_id;
  !!(mvwaddstr win.w i (18 + lineno_sz) line'');
  wattr_set win.w WA.dim color_id;
  !!(mvwaddstr win.w i 0 lineno');
  if trunc then !!(mvwaddstr win.w i (win.xz - 3) "...")

let render_codeblock (win : Win.t) (at : Source.region) : unit =
  let line = at.left.line in
  let (color_id_dflt, color_id_db) = (3, 4) in
  !!(init_pair color_id_dflt Color.white Color.black);
  !!(init_pair color_id_db Color.cyan Color.black);
  let color lineno = if lineno == line then color_id_db else color_id_dflt in
  let colors_f (lineno, line) = (lineno, line, color lineno) in
  let (codeblock, last_line) = codeblock (win.yz - 2) at in
  let lineno_sz = String.length (string_of_int last_line) in
  let codeblock_colored = List.map colors_f codeblock in
  List.iteri (render_codeline win lineno_sz) codeblock_colored

let render (code : t) : unit =
  let win = code.ui.el.data in
  werase win.w;
  render_codeblock win code.at;
  render_location win code.at;
  render_title code.ui.wframe;
  Frame.refresh code.ui
