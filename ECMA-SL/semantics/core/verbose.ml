let log_stmt (s : Stmt.t) : bool =
  match s.it with Skip | Merge | Debug | Block _ -> false | _ -> true

let pp_stmt (fmt : Format.formatter) (s : Stmt.t) : unit =
  match s.it with
  | If (e, _, _) -> Format.fprintf fmt "if (%a) { ..." Expr.pp e
  | While (e, _) -> Format.fprintf fmt "while (%a) { ..." Expr.pp e
  | _ -> Stmt.pp fmt s

module type M = sig
  val eval_expr_val : Expr.t -> Val.t -> unit
  val eval_small_step : Func.t -> Stmt.t -> unit
end

module Disable : M = struct
  let eval_expr_val (_ : Expr.t) (_ : Val.t) : unit = ()
  let eval_small_step (_ : Func.t) (_ : Stmt.t) : unit = ()
end

module Default : M = struct
  let eval_expr_val (e : Expr.t) (v : Val.t) : unit =
    Format.eprintf "» | %a | --> %a@." Expr.pp e Val.pp v

  let eval_small_step (func : Func.t) (s : Stmt.t) : unit =
    if log_stmt s then
      let divider_str = "----------------------------------------" in
      Format.eprintf "%s\nEvaluating >>>> %s [line=%d]: %a@." divider_str
        (Func.name func) s.at.left.line pp_stmt s
end
