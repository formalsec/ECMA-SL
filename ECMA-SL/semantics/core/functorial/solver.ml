open EslBase
include Smtml.Solver.Batch (Smtml.Z3_mappings)

let check solver assumptions =
  Log.debug_k (fun pp ->
    pp "@[<hov 1>      solver:@ %a@]" Smtml.Expr.pp_list assumptions );
  check solver assumptions

let pp_set fmt v =
  let space fmt () = Fmt.fmt fmt "@ " in
  Smtml.Expr.Set.pretty ~pp_sep:space Smtml.Expr.pp fmt v

let check_set solver assumptions =
  Log.debug_k (fun pp -> pp "@[<hov 1>      solver:@ %a@]" pp_set assumptions);
  check_set solver assumptions
