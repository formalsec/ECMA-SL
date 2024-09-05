open EslBase
include Smtml.Solver.Batch (Smtml.Z3_mappings)

let check solver assumptions =
  Log.debug "@[<hov 1>      solver:@ %a@]" Smtml.Expr.pp_list assumptions;
  check solver assumptions
