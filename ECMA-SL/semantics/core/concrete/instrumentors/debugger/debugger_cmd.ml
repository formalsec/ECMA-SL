open Debugger_types

module Message = struct
  let help =
    "Commands:\n\
    \  1: eval <expr>\n\
    \  2: locals\n\
    \  3: step [in|out]\n\
    \  4: continue\n\
    \  5: exit\n\
    \  6: help"

  let missing_expr = "Missing expression for evaluation."
  let invalid_expr = "Invalid expression syntax."
  let invalid_step = "Invalid step. Use 'step', 'step in' or 'step out'."
  let invalid = "Unknown command. Try 'help' for more information."
end

module Interpreter = struct
  let expr_eval_f : (state -> Expr.t -> Val.t) ref =
    ref (fun _ _ -> Val.Str "Unable to evaluate expressions")
end

type t =
  | None
  | Print of string
  | Step
  | StepIn
  | StepOut
  | Continue
  | Exit

let eval_cmd (state : state) (e_tkns : string list) : string =
  if e_tkns == [] then Message.missing_expr
  else
    let e_str = String.concat " " e_tkns in
    try Parsing.parse_expr e_str |> !Interpreter.expr_eval_f state |> Val.str
    with _ -> Message.invalid_expr

let locals_cmd (state : state) : string =
  let locals_f fmt (x, v) =
    if not (String.starts_with ~prefix:"__" x) then
      Fmt.fprintf fmt "%s: %a\n" x Val.pp v
  in
  let (store, _, _) = state in
  Fmt.(asprintf "%a" (pp_hashtbl "" locals_f) store)

let step_cmd (step_args : string list) : t =
  match step_args with
  | [] -> Step
  | "in" :: [] -> StepIn
  | "out" :: [] -> StepOut
  | _ -> Print Message.invalid_step

let execute (state : state) (line : string) : t =
  let tkns = String.trim line |> String.split_on_char ' ' in
  match tkns with
  | [] -> Internal_error.(throw __FUNCTION__ (Invariant "String.split_on_char"))
  | "" :: [] -> None
  | "eval" :: e_tkns -> Print (eval_cmd state e_tkns)
  | "locals" :: [] -> Print (locals_cmd state)
  | "step" :: step_tkns -> step_cmd step_tkns
  | "continue" :: [] -> Continue
  | "exit" :: [] -> Exit
  | "help" :: [] -> Print Message.help
  | _ -> Print Message.invalid
