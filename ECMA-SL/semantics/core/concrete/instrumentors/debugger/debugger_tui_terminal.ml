open Debugger_types
open Debugger_tui_helper
module Command = Debugger_cmd

type colors = { main : Color.t }

type t =
  { prompt : Prompt.t
  ; colors : colors
  ; last_cmd : Command.t
  ; st : state
  }

let colors () : colors = { main = Color.mk Color.black Color.white }

let create (consolewin : Win.t) (execwin : Win.t) : t =
  let (y, x) = (execwin.y + 1, execwin.xz + 1) in
  let yz = consolewin.yz - y in
  let xz = consolewin.xz - x in
  let scrlpad = ScrollPad.mk y x yz xz in
  let prompt = Prompt.mk scrlpad (scrlpad.rows - 1) 4 in
  let colors = colors () in
  let st = (Store.default (), Heap.default (), Call_stack.default ()) in
  { prompt; colors; last_cmd = None; st }

let resize (term : t) (consolewin : Win.t) (execwin : Win.t) : t =
  let (y, x) = (execwin.y + 1, execwin.xz + 1) in
  let yz = consolewin.yz - y in
  let xz = consolewin.xz - x in
  let scrlpad = ScrollPad.mk y x yz xz in
  let prompt = Prompt.mk scrlpad (scrlpad.rows - 1) 4 in
  { term with prompt }

let window (term : t) : window = Prompt.window term.prompt
let refresh (term : t) : unit = Prompt.refresh term.prompt
let rec element (term : t) : t element = { v = term; window; refresh; element }
let set_data (term : t) (st : state) : t = { term with last_cmd = None; st }
let get_last_cmd (term : t) : Command.t = term.last_cmd

let render_background (scrlpad : ScrollPad.t) (colors : colors) : unit =
  wattr_set scrlpad.w WA.(combine [ dim; standout ]) colors.main;
  draw_rectangle scrlpad.w 0 0 scrlpad.rows scrlpad.xz ' '

let render_prompt (scrlpad : ScrollPad.t) (colors : colors) : unit =
  wattr_set scrlpad.w WA.(combine [ dim; standout ]) colors.main;
  !!(mvwaddstr scrlpad.w (scrlpad.first + scrlpad.yz - 1) 0 ">>>")

let render_static (term : t) : unit =
  render_background term.prompt.scrlpad term.colors;
  render_prompt term.prompt.scrlpad term.colors

let print (prompt : Prompt.t) (msg : string) : unit =
  let print_line line =
    !!(mvwaddstr prompt.scrlpad.w prompt.y 0 line);
    if String.length line < prompt.scrlpad.xz then Prompt.scrldown prompt
    else Prompt.scrldown_fill prompt 1
  in

  String.trim msg
  |> String.split_on_char '\n'
  |> List.map (String.split_at_length prompt.scrlpad.xz)
  |> List.flatten
  |> List.iter print_line

let update (term : t) (prompt : Prompt.t) (cmd : Command.t) : t =
  (match cmd with Print msg -> print prompt msg | _ -> ());
  { term with prompt; last_cmd = cmd }

let cursor (term : t) : unit =
  !!(wmove term.prompt.scrlpad.w term.prompt.y term.prompt.x);
  !!(curs_set 1)

let enter (term : t) : t =
  Prompt.scrldown term.prompt;
  let (prompt, command) = Prompt.reset term.prompt in
  let cmd = Command.execute term.st command in
  let term' = update term prompt cmd in
  Prompt.scrldown term'.prompt;
  render_prompt term'.prompt.scrlpad term'.colors;
  cursor term';
  term'

let process_input (term : t) (input : int) : t =
  if input == Key.enter || input == 10 then enter term
  else
    let prompt' = Prompt.process term.prompt input in
    { term with prompt = prompt' }

let callback (term : t) (input : int) : t =
  let scrlpad = term.prompt.scrlpad in
  wattr_set scrlpad.w WA.(combine [ dim; standout ]) term.colors.main;
  let term' = process_input term input in
  refresh term';
  term'
