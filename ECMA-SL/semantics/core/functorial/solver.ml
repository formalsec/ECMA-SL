include Smtml.Solver.Batch (Smtml.Z3_mappings)

let check solver assumptions =
  Logs.debug (fun k ->
    k "@[<hov 1>      solver:@ %a@]" Smtml.Expr.pp_list assumptions );
  check solver assumptions

let pp_set fmt v = Smtml.Expr.Set.pretty ~pp_sep:Fmt.sp Smtml.Expr.pp fmt v

let check_set solver assumptions =
  Logs.debug (fun k -> k "@[<hov 1>      solver:@ %a@]" pp_set assumptions);
  check_set solver assumptions
