type value = Val.t
type obj = value Object.t
type store = value Store.t
type heap = value Heap.t
type stack = store Call_stack.t

module Show = struct
  let header () : unit =
    Fmt.printf "\n%a"
      (Font.pp_text_out [ Font.Cyan ])
      "----------------------------------------\n\
      \       Core ECMA-SL Debug Prompt\n\
       ----------------------------------------\n"

  let footer () : unit =
    Fmt.printf "\n%a\n@."
      (Font.pp_text_out [ Font.Cyan ])
      "----------------------------------------"

  let dialog () : unit =
    Fmt.printf "\n%a\n%s"
      (Font.pp_text_out [ Font.Cyan ])
      "Commands:"
      "  1: eval <var|$loc_n|obj.fld>\n\
      \  2: store\n\
      \  3: heap\n\
      \  4: stack\n\
      \  5: help\n\
      \  6: step [in|out]\n\
      \  7: continue\n\
      \  8: exit"

  let prompt () : unit =
    Fmt.printf "\n\n%a @?" (Font.pp_text_out [ Font.Faint ]) ">>>"
end

exception Cmd_error of string

let cmd_err (msg : string) : 'a = raise (Cmd_error msg)

let ( !! ) (res : ('a, string) Result.t) : 'a =
  match res with Ok v -> v | Error msg -> cmd_err msg

type cmd =
  | Eval of string
  | Store
  | Heap
  | Stack
  | Help
  | Step
  | StepIn
  | StepOut
  | Continue
  | Exit

let parse_command (line : string) : cmd option =
  let tkns = String.split_on_char ' ' line in
  match tkns with
  | [] -> None
  | [ "eval" ] -> Some (Eval "")
  | [ "eval"; expr ] -> Some (Eval expr)
  | [ "store" ] -> Some Store
  | [ "heap" ] -> Some Heap
  | [ "stack" ] -> Some Stack
  | [ "help" ] -> Some Help
  | [ "step" ] -> Some Step
  | [ "step"; "in" ] -> Some StepIn
  | [ "step"; "out" ] -> Some StepOut
  | [ "continue" ] -> Some Continue
  | [ "exit" ] -> Some Exit
  | _ -> None

let print_stmt (f : Func.t) (s : Stmt.t) : unit =
  let lineno_str = string_of_int s.at.left.line in
  let lineno_sz = String.length lineno_str in
  let lineno_indent = String.make lineno_sz ' ' in
  let pp_stmt = Font.pp_out [ Font.Cyan ] Stmt.pp_simple in
  Fmt.printf "\n%s | %a\n%s |    %a\n%s | }" lineno_indent Func.pp_simple f
    lineno_str pp_stmt s lineno_indent

let print_obj (obj : obj) : unit = Fmt.printf "%a" (Object.pp Val.pp) obj

let print_val (heap : heap) (res : value) : unit =
  match res with
  | Loc l ->
    !!(Heap.get heap l) |> print_obj;
    Fmt.printf " %a" (Font.pp_text_out [ Font.Faint ]) ("// " ^ Loc.str l)
  | v -> Fmt.printf "%a" Val.pp v

let eval_fld (heap : heap) (lv : Val.t) (fn : string) : Val.t =
  let _fld_val v_opt = Option.value ~default:(Val.Symbol "undefined") v_opt in
  match lv with
  | Loc l -> _fld_val (Heap.get_field_opt heap l fn)
  | _ -> cmd_err (Fmt.asprintf "Invalid location value '%a'." Val.pp lv)

let eval_cmd (store : store) (heap : heap) (expr : string) () : unit =
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

let store_cmd (store : store) () : unit =
  Fmt.printf "%a" (Store.pp_tabular Val.pp) store

let heap_cmd (heap : heap) () : unit =
  Fmt.printf "%a" (Heap.pp_tabular (Object.pp Val.pp)) heap

let stack_cmd (stack : stack) () : unit =
  Fmt.printf "Currently at %a" Call_stack.pp_tabular stack

let help_cmd : unit -> unit = Show.dialog
let invalid_cmd () : unit = cmd_err "Invalid command. Try again."

let rec debug_loop (store : store) (heap : heap) (stack : stack) : cmd =
  let run_cmd cmd = cmd () |> fun () -> debug_loop store heap stack in
  Show.prompt ();
  let command = read_line () |> parse_command in
  match command with
  | Some (Eval expr) -> run_cmd @@ eval_cmd store heap expr
  | Some Store -> run_cmd @@ store_cmd store
  | Some Heap -> run_cmd @@ heap_cmd heap
  | Some Stack -> run_cmd @@ stack_cmd stack
  | Some Help -> run_cmd @@ help_cmd
  | Some flow_cmd -> flow_cmd
  | None -> run_cmd @@ invalid_cmd

and debug_loop_safe (store : store) (heap : heap) (stack : stack) : cmd =
  try debug_loop store heap stack
  with Cmd_error err ->
    Fmt.printf "%s" err;
    debug_loop_safe store heap stack

module type M = sig
  type t

  val initialize : unit -> t

  val run :
    store * heap * stack * t -> Stmt.t -> Stmt.t list -> t * stack * Stmt.t list

  val custom_inject :
    Stmt.t -> t -> stack -> Stmt.t list -> t * stack * Stmt.t list
end

module Disable : M = struct
  type t = unit

  let initialize () : t = ()

  let run ((_, _, stack, db) : store * heap * stack * t) (s : Stmt.t)
    (cont : Stmt.t list) : t * stack * Stmt.t list =
    (db, stack, s :: cont)

  let custom_inject (_ : Stmt.t) (_ : t) (stack : stack) (cont : Stmt.t list) :
    t * stack * Stmt.t list =
    ((), stack, cont)
end

module Enable : M = struct
  type t =
    | Initial
    | Normal
    | Call
    | Final

  let show_initial_state () : unit =
    Show.header ();
    Show.dialog ();
    Fmt.printf "\n"

  let show_final_state () : unit = Show.footer ()

  let debug_prompt (store : store) (heap : heap) (stack : stack) (state : t)
    (s : Stmt.t) : cmd =
    if state = Final then Exit
    else (
      if state = Initial then show_initial_state ();
      print_stmt (Call_stack.func stack) s;
      let cmd = debug_loop_safe store heap stack in
      if cmd = Exit then show_final_state ();
      cmd )

  let rec inject_debug_outerscope (stack : stack) : stack =
    let open Call_stack in
    match pop stack with
    | (Toplevel _, _) -> stack
    | (Intermediate (loc, restore), stack') ->
      let { func; _ } = loc in
      let { store; cont; retvar } = restore in
      let (stack'', cont') = inject_debug_innerscope stack' cont in
      push stack'' func store cont' retvar

  and inject_debug_innerscope (stack : stack) (cont : Stmt.t list) :
    stack * Stmt.t list =
    match cont with
    | ({ it = Skip; _ } as s) :: cont' | ({ it = Merge; _ } as s) :: cont' ->
      let (stack', cont'') = inject_debug_innerscope stack cont' in
      (stack', s :: cont'')
    | { it = Block stmts; _ } :: cont' ->
      let (stack', cont'') = inject_debug_innerscope stack (stmts @ cont') in
      (stack', cont'')
    | { it = Debug _; _ } :: _ -> (stack, cont)
    | s :: cont' -> (stack, Source.(Stmt.Debug s @> s.at) :: cont')
    | [] -> (inject_debug_outerscope stack, cont)

  let update_prog (cmd : cmd) (stack : stack) (s : Stmt.t) (cont : Stmt.t list)
    : t * stack * Stmt.t list =
    match (cmd, s) with
    | (StepIn, { it = AssignCall _; _ }) -> (Call, stack, s :: cont)
    | (Step, _) | (StepIn, _) ->
      inject_debug_innerscope stack cont |> fun (stack', cont') ->
      (Normal, stack', s :: cont')
    | (StepOut, _) ->
      inject_debug_outerscope stack |> fun stack' -> (Normal, stack', s :: cont)
    | (Continue, _) -> (Normal, stack, s :: cont)
    | (Exit, _) -> (Final, stack, s :: cont)
    | _ -> Eslerr.(internal __FUNCTION__ (Expecting "termination cmd"))

  let initialize () : t = Initial

  let run ((store, heap, stack, db) : store * heap * stack * t) (s : Stmt.t)
    (cont : Stmt.t list) : t * stack * Stmt.t list =
    let cmd = debug_prompt store heap stack db s in
    update_prog cmd stack s cont

  let custom_inject (s : Stmt.t) (db : t) (stack : stack) (cont : Stmt.t list) :
    t * stack * Stmt.t list =
    match (db, s) with
    | (Call, { it = AssignCall _; _ }) ->
      inject_debug_innerscope stack cont |> fun (stack', cont') ->
      (Normal, stack', cont')
    | _ -> (Normal, stack, cont)
end
