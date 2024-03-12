open EslCore
open EslSyntax

let log_stmt (s : Stmt.t) : bool =
  match s.it with Skip | Merge | Block _ -> false | _ -> true

module type M = sig
  val eval_expr_val : Expr.t -> Val.t -> unit
  val eval_small_step : Func.t -> Stmt.t -> unit
end

module Disable : M = struct
  let eval_expr_val (_ : Expr.t) (_ : Val.t) : unit = ()
  let eval_small_step (_ : Func.t) (_ : Stmt.t) : unit = ()
end

module Enable : M = struct
  let print_source (fmt : Fmt.t) (at : Source.region) : unit =
    if false then Fmt.fprintf fmt " @@ %a" Source.pp_region at else ()

  let eval_expr_val (e : Expr.t) (v : Val.t) : unit =
    Fmt.eprintf "» | %a | --> %a%a@." Expr.pp e Val.pp v print_source e.at

  let eval_small_step (f : Func.t) (s : Stmt.t) : unit =
    if log_stmt s then
      let divider = "----------------------------------------" in
      Fmt.eprintf "%s\nEvaluating >>>> %a() [line=%d]: %a%a@." divider Id.pp
        (Func.name f) s.at.left.line Stmt.pp_simple s print_source s.at
end
