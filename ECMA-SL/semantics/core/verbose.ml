module StmtVerbose = struct
  open Stmt

  let log_stmt (s : Stmt.t) : bool =
    match s.it with Skip | Merge | Debug | Block _ -> false | _ -> true

  let pp (fmt : Format.formatter) (s : Stmt.t) : unit =
    match s.it with
    | If (e, _, _) -> Format.fprintf fmt "if (%a) { ..." Expr.pp e
    | While (e, _) -> Format.fprintf fmt "while (%a) { ..." Expr.pp e
    | _ -> Stmt.pp fmt s
end

type t =
  { mutable eval_expr : Expr.t -> Val.t -> unit
  ; mutable eval_small_step : Func.t -> Stmt.t -> unit
  }

let verbose : t =
  { eval_expr = (fun _ _ -> ()); eval_small_step = (fun _ _ -> ()) }

let log_eval_expr (e : Expr.t) (v : Val.t) : unit =
  Format.eprintf "» | %a | --> %a@." Expr.pp e Val.pp v

let log_eval_small_step (func : Func.t) (s : Stmt.t) : unit =
  if StmtVerbose.log_stmt s then
    let divider_str = "----------------------------------------" in
    Format.eprintf "%s\nEvaluating >>>> %s [line=%d]: %a@." divider_str
      (Func.name func) s.at.left.line StmtVerbose.pp s

let init () : unit =
  if !Config.Interpreter.verbose then (
    verbose.eval_expr <- log_eval_expr;
    verbose.eval_small_step <- log_eval_small_step )

let eval_expr (e : Expr.t) (v : Val.t) : unit = verbose.eval_expr e v

let eval_small_step (func : Func.t) (s : Stmt.t) : unit =
  verbose.eval_small_step func s
