open Ecma_sl

exception Crash of Source.at * string
exception Invalid_arg of Source.at * string

include Interpreter_functor.Make (Symbolic)

let link_env filename prog =
  let module Env = Symbolic.Env in
  let env0 = Env.Build.empty () |> Env.Build.add_functions prog in
  Env.Build.add_extern_functions (Symbolic_esl_ffi.extern_cmds env0) env0
  |> Env.Build.add_extern_functions Symbolic_esl_ffi.concrete_api
  |> Env.Build.add_extern_functions (Symbolic_esl_ffi.symbolic_api filename)

let check_return_value thread = function
  | Ok result -> (
    match List.map Smtml.Expr.view result with
    | [ List [ Hc.{ node = Smtml.Expr.Val False; _ }; result ] ] ->
      let mem = Symbolic.Thread.mem thread in
      Logs.app (fun k ->
        k "- : %a =@[<hov> %a@]" Smtml.Ty.pp (Smtml.Expr.ty result)
          (Symbolic.Memory.pp_val mem)
          result );
      Ok ()
    | [ List [ { node = Val True; _ }; e ] ] ->
      let msg =
        Fmt.str "Failure: @[<hov>uncaught exception:@ %a@]" Smtml.Expr.pp e
      in
      Logs.app (fun k -> k "%s" msg);
      Error (`Failure msg)
    | _ ->
      let msg =
        Fmt.str "Failure: @[<hov>something went terribly wrong:@ %a@]"
          Smtml.Expr.pp_list result
      in
      Logs.app (fun k -> k "%s" msg);
      Error (`Failure msg) )
  | Error _ as err -> err

let run ?(no_stop_at_failure = false) ?(target = "main") ?(callback = ignore)
  filename prog =
  let start = Sys.time () in
  let env = link_env filename prog in
  let computation = main env target in
  let thread = Choice_monad.Thread.create () in
  let results = Symbolic.Choice.run computation thread in
  let report =
    { Symbolic_report.filename
    ; execution_time = start
    ; solver_time = 0.0
    ; solver_queries = 0
    ; num_failures = 0
    ; failures = []
    }
  in
  let result =
    let exception Exit in
    let exit = ref None in
    let () =
      try
        results (fun (result, thread) ->
          let result = check_return_value thread result in
          (* BAD: ignoring return value because I don't care about the result *)
          let _ = callback (result, thread) in
          match result with
          | Ok () -> ()
          | Error witness ->
            Logs.app (fun k -> k "%a" Symbolic_error.pp witness);
            report.num_failures <- succ report.num_failures;
            report.failures <- witness :: report.failures;
            if no_stop_at_failure then ()
            else begin
              exit := Some witness;
              raise Exit
            end )
      with Exit -> ()
    in
    report.execution_time <- Sys.time () -. report.execution_time;
    report.solver_time <- !Solver.solver_time;
    report.solver_queries <- !Solver.solver_count;
    match !exit with None -> Ok () | Some err -> Error err
  in
  (result, report)
