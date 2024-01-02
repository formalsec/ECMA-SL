module StmtVerbose = struct
  open Stmt

  let log_stmt (s : Stmt.t) : bool =
    match s.it with Skip | Merge | Block _ -> false | _ -> true

  let str ?(expr_printer : Expr.t -> string = Expr.str) (s : t) : string =
    let _str_e e = expr_printer e in
    let _str_es es = String.concat ", " (List.map _str_e es) in
    match s.it with
    | If (e, _, _) -> Printf.sprintf "if (%s) { ..." (_str_e e)
    | While (e, _) -> Printf.sprintf "while (%s) { ..." (_str_e e)
    | _ -> Stmt.str s
end

type t =
  { mutable eval_expr : Expr.t -> Val.t -> unit
  ; mutable eval_small_step : Func.t -> Stmt.t -> unit
  }

let verbose : t =
  { eval_expr = (fun _ _ -> ()); eval_small_step = (fun _ _ -> ()) }

let log_eval_expr (e : Expr.t) (v : Val.t) : unit =
  Printf.eprintf " - '%s' = %s\n" (Expr.str e) (Val.str v)

let log_eval_small_step (func : Func.t) (s : Stmt.t) : unit =
  if StmtVerbose.log_stmt s then
    let divider_str = "----------------------------------------" in
    Printf.eprintf "%s\nEvaluating >>>> %s [line=%d]: %s\n" divider_str
      (Func.name func) s.at.left.line (StmtVerbose.str s)

let init () : unit =
  if !Config.Interpreter.verbose then (
    verbose.eval_expr <- log_eval_expr;
    verbose.eval_small_step <- log_eval_small_step )

let eval_expr (e : Expr.t) (v : Val.t) : unit = verbose.eval_expr e v

let eval_small_step (func : Func.t) (s : Stmt.t) : unit =
  verbose.eval_small_step func s
