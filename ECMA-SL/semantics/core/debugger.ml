type value = Val.t
type obj = value Object.t
type store = value Store.t
type heap = value Heap.t
type stack = store Call_stack.t

exception Cmd_error of string

let cmd_err (msg : string) : 'a = raise (Cmd_error msg)

let ( !! ) (res : ('a, string) Result.t) : 'a =
  match res with Ok v -> v | Error msg -> cmd_err msg

let show_header () : unit =
  Format.printf "\n%s"
    (Font.str_format_out [ Font.Cyan ]
       "----------------------------------------\n\
       \       Core ECMA-SL Debug Prompt\n\
        ----------------------------------------\n" )

let show_end () : unit =
  Format.printf "\n%s\n\n"
    (Font.str_format_out [ Font.Cyan ]
       "----------------------------------------" )

let show_dialog () : unit =
  Format.printf "\n%s%s"
    (Font.str_format_out [ Font.Cyan ] "Commands:\n")
    "  1: eval <var|$loc_n|obj.fld>\n\
    \  2: store\n\
    \  3: heap\n\
    \  4: stack\n\
    \  5: continue\n\
    \  6: help"

let show_command_prompt () : unit =
  Format.printf "\n\n%s " (Font.str_format_out [ Font.Faint ] ">>>")

type t =
  | Eval of string
  | Store
  | Heap
  | Stack
  | Continue
  | Help

let parse_command (line : string) : t option =
  let tkns = String.split_on_char ' ' line in
  match tkns with
  | [] -> None
  | [ "eval" ] -> Some (Eval "")
  | [ "eval"; expr ] -> Some (Eval expr)
  | [ "store" ] -> Some Store
  | [ "heap" ] -> Some Heap
  | [ "stack" ] -> Some Stack
  | [ "continue" ] -> Some Continue
  | [ "help" ] -> Some Help
  | _ -> None

let print_obj (obj : obj) : unit = Format.printf "%s" (Object.str Val.pp obj)

let print_val (heap : heap) (res : value) : unit =
  match res with
  | Loc l ->
    !!(Heap.get heap l) |> print_obj;
    Format.printf " %s" (Font.str_format_out [ Font.Faint ] ("// " ^ Loc.str l))
  | v -> Format.printf "%s" (Val.str v)

let eval_fld (heap : heap) (lv : Val.t) (fn : string) : Val.t =
  let _fld_val v_opt = Option.value ~default:(Val.Symbol "undefined") v_opt in
  match lv with
  | Loc l -> _fld_val (Heap.get_field_opt heap l fn)
  | _ -> cmd_err (Format.sprintf "Invalid location value '%s'." (Val.str lv))

let eval_cmd (store : store) (heap : heap) (expr : string) : unit =
  let args = String.split_on_char '.' expr in
  match args with
  | [] | [ "" ] -> cmd_err "Missing eval expression."
  | [ x ] ->
    if String.starts_with ~prefix:"$" x then !!(Heap.get heap x) |> print_obj
    else !!(Store.get store x) |> print_val heap
  | oe :: fns ->
    let lv =
      if String.starts_with ~prefix:"$" oe then Val.Loc oe
      else !!(Store.get store oe)
    in
    List.fold_left (eval_fld heap) lv fns |> print_val heap

let store_cmd (store : store) : unit =
  Format.printf "%s" (Store.str Val.pp store)

let heap_cmd (heap : heap) : unit =
  Format.printf "%s" (Heap.str (Object.pp Val.pp) heap)

let stack_cmd (stack : stack) : unit = Format.printf "%s" (Call_stack.str stack)
let help_cmd () : unit = show_dialog ()
let invalid_cmd () : unit = cmd_err "Invalid command. Try again."

let rec debug_loop (store : store) (heap : heap) (stack : stack) : unit =
  let run_cmd cmd = cmd () |> fun () -> debug_loop store heap stack in
  show_command_prompt ();
  let command = read_line () |> parse_command in
  match command with
  | Some (Eval expr) -> run_cmd (fun () -> eval_cmd store heap expr)
  | Some Store -> run_cmd (fun () -> store_cmd store)
  | Some Heap -> run_cmd (fun () -> heap_cmd heap)
  | Some Stack -> run_cmd (fun () -> stack_cmd stack)
  | Some Continue -> ()
  | Some Help -> run_cmd (fun () -> help_cmd ())
  | None -> run_cmd (fun () -> invalid_cmd ())

and debug_loop_safe (store : store) (heap : heap) (stack : stack) : unit =
  try debug_loop store heap stack
  with Cmd_error err ->
    Format.printf "%s" err;
    debug_loop_safe store heap stack

let run (store : store) (heap : heap) (stack : stack) : unit =
  show_header ();
  show_dialog ();
  debug_loop_safe store heap stack;
  show_end ()
