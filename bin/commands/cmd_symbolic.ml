open Smtml_prelude.Result
module Choice = Ecma_sl_symbolic.Choice_monad.Seq
module Thread = Ecma_sl_symbolic.Choice_monad.Thread
module Solver = Ecma_sl_symbolic.Solver

module Symbolic_interpreter =
  Ecma_sl_symbolic.Symbolic_interpreter.Make (Ecma_sl_symbolic.Symbolic_error)

let valid_languages : Enums.Lang.t list = Enums.Lang.[ Auto; JS; ESL; CESL ]

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
  let valid_langs = Enums.Lang.valid_langs valid_languages lang in
  match Enums.Lang.resolve_file_lang valid_langs fpath with
  | Some CESL -> Cmd_compile.load fpath
  | Some ESL -> Cmd_compile.compile true fpath
  | Some JS -> prog_of_js fpath
  | _ ->
    let msg = Fmt.str "%a :unreconized file type" Fpath.pp fpath in
    Result.error (`Generic msg)

let serialize_thread workspace =
  let open Ecma_sl in
  let mode = 0o666 in
  let (next_int, _) = Base.make_counter 0 1 in
  fun thread witness ->
    let open Fpath in
    let pc = Thread.pc thread in
    Logs.debug (fun pp -> pp "@[<hov 1>  path cond :@ %a@]" Solver.pp_set pc);
    let solver = Thread.solver thread in
    match Solver.check_set solver pc with
    | `Unsat | `Unknown ->
      (* Should not happen. But it does? *)
      [ witness ]
    | `Sat ->
      let model = Solver.model solver in
      let path = Fmt.kstr (add_seg workspace) "witness-%d" (next_int ()) in
      let pp = Fmt.option Smtml.Model.pp in
      let pc = Smtml.Expr.Set.to_list @@ pc in
      let _ = Bos.OS.File.writef ~mode (path + ".js") "%a" pp model in
      let _ =
        Bos.OS.File.writef ~mode (path + ".smtml") "%a" Smtml.Expr.pp_smt pc
      in
      [ witness ]

let write_report workspace symbolic_report =
  let mode = 0o666 in
  let json = Symbolic_interpreter.Symbolic_result.to_json symbolic_report in
  let path = Fpath.(workspace / "symbolic-execution.json") in
  Result.bos
  @@ Bos.OS.File.writef ~mode path "%a" (Yojson.pretty_print ~std:true) json

let run ~input ~lang ~target ~workspace =
  let* prog = dispatch_prog lang input in
  let testsuite = Fpath.(workspace / "test-suite") in
  let* _ = Result.bos (Bos.OS.Dir.create ~mode:0o777 testsuite) in
  let (result, report) =
    Symbolic_interpreter.run ~no_stop_at_failure:false ~target
      ~callback:(serialize_thread testsuite)
      input prog
  in
  Logs.debug (fun k -> k "  exec time : %fs" report.execution_time);
  Logs.debug (fun k -> k "solver time : %fs" report.solver_time);
  if report.num_failures = 0 then Logs.app (fun k -> k "All Ok!")
  else Logs.app (fun k -> k "Found %d problems!" report.num_failures);
  let* () = write_report workspace report in
  match result with Ok () -> Ok () | Error err -> Error (`Symbolic err)
