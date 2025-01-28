open Smtml_prelude.Result
module Value = Ecma_sl_symbolic.Symbolic_value
module Choice = Ecma_sl_symbolic.Choice_monad.Seq
module Thread = Ecma_sl_symbolic.Choice_monad.Thread
module Env = Ecma_sl_symbolic.Symbolic.P.Env
module Extern_func = Ecma_sl_symbolic.Symbolic.P.Extern_func
module Memory = Ecma_sl_symbolic.Symbolic_memory
module State = Ecma_sl_symbolic.Symbolic_interpreter.State
module Solver = Ecma_sl_symbolic.Solver

module Options = struct
  let langs : Enums.Lang.t list = Enums.Lang.[ Auto; JS; ESL; CESL ]

  type t =
    { input : Fpath.t
    ; lang : Enums.Lang.t
    ; target : string
    ; workspace : Fpath.t
    }

  let set (input : Fpath.t) (lang : Enums.Lang.t) (target : string)
    (workspace : Fpath.t) : t =
    { input; lang; target; workspace }
end

let prog_of_js fpath =
  let open Ecma_sl in
  let interp = Share.es6_sym_interp () in
  let prog = Parsing.parse_prog interp in
  (* We can use interpreter will all the metadata once the value_translator can handle nary operators *)
  (* let instrument = Cmd_interpret.Options.default_instrument () in *)
  (* let* (interp, _) = Cmd_execute.setup_execution ECMARef6Sym None instrument in *)
  let ast = Fpath.v (Filename.temp_file "ecmasl" "ast.cesl") in
  let* () = Cmd_encode.encode None fpath (Some ast) in
  let+ build_ast = Cmd_execute.build_ast ast in
  Hashtbl.replace (Prog.funcs prog) (Func.name' build_ast) build_ast;
  prog

let dispatch_prog lang fpath =
  let valid_langs = Enums.Lang.valid_langs Options.langs lang in
  match Enums.Lang.resolve_file_lang valid_langs fpath with
  | Some CESL -> Cmd_compile.load fpath
  | Some ESL -> Cmd_compile.compile true fpath
  | Some JS -> prog_of_js fpath
  | _ ->
    let msg = Fmt.str "%a :unreconized file type" Fpath.pp fpath in
    Result.error (`Generic msg)

let link_env filename prog =
  let open Ecma_sl_symbolic in
  let env0 = Env.Build.empty () |> Env.Build.add_functions prog in
  Env.Build.add_extern_functions (Symbolic_esl_ffi.extern_cmds env0) env0
  |> Env.Build.add_extern_functions Symbolic_esl_ffi.concrete_api
  |> Env.Build.add_extern_functions (Symbolic_esl_ffi.symbolic_api filename)

let serialize_thread workspace =
  let open Ecma_sl in
  let mode = 0o666 in
  let (next_int, _) = Base.make_counter 0 1 in
  fun result thread ->
    let open Fpath in
    let pc = Thread.pc thread in
    Logs.debug (fun pp -> pp "@[<hov 1>  path cond :@ %a@]" Solver.pp_set pc);
    let solver = Thread.solver thread in
    match Solver.check_set solver pc with
    | `Unsat | `Unknown ->
      (* Should not happen. But it does? *)
      Ok ()
    | `Sat ->
      let model = Solver.model solver in
      let path =
        Fmt.kstr
          (add_seg (workspace / "test-suite"))
          (match result with Ok () -> "testcase-%d" | Error _ -> "witness-%d")
          (next_int ())
      in
      let pp = Fmt.option Smtml.Model.pp in
      let pc = Smtml.Expr.Set.to_list @@ pc in
      Result.bos
      @@
      let* () = Bos.OS.File.writef ~mode (path + ".js") "%a" pp model in
      Bos.OS.File.writef ~mode (path + ".smtml") "%a" Smtml.Expr.pp_smt pc

let process_result (workspace : Fpath.t) (result, thread) =
  let parse_return_value = function
    | Ok result -> (
      (* FIXME: Maybe move this prints to some better place *)
      match List.map Smtml.Expr.view result with
      | [ List [ Hc.{ node = Smtml.Expr.Val False; _ }; result ] ] ->
        let mem = Thread.mem thread in
        Logs.app (fun k ->
          k "- : %a =@[<hov> %a@]" Smtml.Ty.pp (Smtml.Expr.ty result)
            (Memory.pp_val mem) result );
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
  in
  let result = parse_return_value result in
  let+ () = serialize_thread workspace result thread in
  result

let write_report workspace symbolic_report =
  let mode = 0o666 in
  let json = Ecma_sl_symbolic.Symbolic_report.to_json symbolic_report in
  let path = Fpath.(workspace / "symbolic-execution.json") in
  Result.bos
  @@ Bos.OS.File.writef ~mode path "%a" (Yojson.pretty_print ~std:true) json

let no_stop_at_failure = false

let run () (opts : Options.t) : unit Result.t =
  let* p = dispatch_prog opts.lang opts.input in
  let env = link_env opts.input p in
  let start = Stdlib.Sys.time () in
  let thread = Thread.create () in
  let result = Ecma_sl_symbolic.Symbolic_interpreter.main env opts.target in
  let results = Choice.run result thread in
  let execution_time = Stdlib.Sys.time () -. start in
  let solver_time = !Solver.solver_time in
  let solver_queries = !Solver.solver_count in
  let testsuite = Fpath.(opts.workspace / "test-suite") in
  let* _ = Result.bos (Bos.OS.Dir.create ~mode:0o777 testsuite) in
  let report =
    { Ecma_sl_symbolic.Symbolic_report.filename = opts.input
    ; execution_time
    ; solver_time
    ; solver_queries
    ; num_failures = 0
    ; failures = []
    }
  in
  let print_and_count_failures results =
    let exception Exit in
    (* We have to measure execution time here as well *)
    let exit = ref None in
    let () =
      try
        results (fun result ->
          let start_time = Stdlib.Sys.time () in
          report.execution_time <-
            report.execution_time +. (Stdlib.Sys.time () -. start_time);
          let ret =
            let* result = process_result opts.workspace result in
            match result with
            | Ok () -> Ok ()
            | Error witness ->
              Logs.app (fun k ->
                k "%a" Ecma_sl_symbolic.Symbolic_error.pp witness );
              report.num_failures <- succ report.num_failures;
              report.failures <- witness :: report.failures;
              if no_stop_at_failure then Ok () else Error (`Symbolic witness)
          in
          match ret with
          | Ok () -> ()
          | Error err ->
            exit := Some err;
            raise Exit )
      with Exit -> ()
    in
    match !exit with None -> Ok () | Some err -> Error err
  in
  let result = print_and_count_failures results in
  if report.num_failures = 0 then Logs.app (fun k -> k "All Ok!")
  else Logs.app (fun k -> k "Found %d problems!" report.num_failures);
  Logs.debug (fun k -> k "  exec time : %fs" report.execution_time);
  Logs.debug (fun k -> k "solver time : %fs" report.solver_time);
  let* () = write_report opts.workspace report in
  result
