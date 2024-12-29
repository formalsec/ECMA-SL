open Smtml_prelude.Result
module Thread = Ecma_sl.Choice_monad.Thread
module Env = Ecma_sl.Symbolic.P.Env
module Value = Ecma_sl.Symbolic.P.Value
module Choice = Ecma_sl.Symbolic.P.Choice
module Extern_func = Ecma_sl.Symbolic.P.Extern_func
module State = Ecma_sl.Symbolic_interpreter.State
module Solver = Ecma_sl.Solver

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
  let interp = Share.es6_sym_interp () |> Parsing.parse_prog in
  (* We can use interpreter will all the metadata once the value_translator can handle nary operators *)
  (* let instrument = Cmd_interpret.Options.default_instrument () in *)
  (* let* (interp, _) = Cmd_execute.setup_execution ECMARef6Sym None instrument in *)
  let ast = Fpath.v (Filename.temp_file "ecmasl" "ast.cesl") in
  let* () = Cmd_encode.encode None fpath (Some ast) in
  let+ build_ast = Cmd_execute.build_ast ast in
  Hashtbl.replace (Prog.funcs interp) (Func.name' build_ast) build_ast;
  interp

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
  let open Ecma_sl in
  let env0 = Env.Build.empty () |> Env.Build.add_functions prog in
  Env.Build.add_extern_functions (Symbolic_esl_ffi.extern_cmds env0) env0
  |> Env.Build.add_extern_functions Symbolic_esl_ffi.concrete_api
  |> Env.Build.add_extern_functions (Symbolic_esl_ffi.symbolic_api filename)

let pp_model fmt model =
  let pp_map fmt (s, v) =
    Fmt.pf fmt {|"%a" : %a|} Smtml.Symbol.pp s Smtml.Value.pp v
  in
  let pp_vars = Fmt.list ~sep:Fmt.comma pp_map in
  Fmt.pf fmt "@[<v 2>module.exports.symbolic_map =@ { %a@\n}@]" pp_vars
    (Smtml.Model.get_bindings model)

type witness =
  [ `SymAbort of string
  | `SymAssertFailure of Extern_func.value
  | `SymExecFailure of Extern_func.value
  | `SymEvalFailure of Extern_func.value
  | `SymReadFileFailure of Extern_func.value
  ]

let serialize_thread (workspace : Fpath.t) :
  witness option -> Thread.t -> unit Result.t =
  let open Ecma_sl in
  let mode = 0o666 in
  let (next_int, _) = Base.make_counter 0 1 in
  fun witness thread ->
    let open Fpath in
    let pc = Thread.pc thread in
    Logs.debug (fun pp -> pp "@[<hov 1>  path cond :@ %a@]" Solver.pp_set pc);
    let solver = Thread.solver thread in
    match Solver.check_set solver pc with
    | `Unsat | `Unknown ->
      (* Should not happen. But it does? This is so buggy omg *)
      Ok ()
    | `Sat ->
      let model = Solver.model solver in
      let path =
        Fmt.ksprintf
          (add_seg (workspace / "test-suite"))
          (match witness with None -> "testcase-%d" | Some _ -> "witness-%d")
          (next_int ())
      in
      let pp = Fmt.pp_opt pp_model in
      let pc = Smtml.Expr.Set.to_list @@ pc in
      Result.bos
      @@
      let* () = Bos.OS.File.writef ~mode (path + ".js") "%a" pp model in
      Bos.OS.File.writef ~mode (path + ".smtml") "%a" Smtml.Expr.pp_smt pc

let process_result (workspace : Fpath.t) (res, thread) : witness option Result.t
    =
  let process_witness = function
    | Ok result -> (
      (* FIXME: Maybe move this prints to some better place *)
      match List.map Smtml.Expr.view result with
      | [ List [ Hc.{ node = Smtml.Expr.Val False; _ }; result ] ] ->
        Logs.app (fun k ->
          k "- : %a = %a" Smtml.Ty.pp (Smtml.Expr.ty result) Smtml.Expr.pp
            result );
        Ok None
      | [ List [ { node = Val True; _ }; e ] ] ->
        let msg =
          Fmt.str "Failure: @[<hov>uncaught exception:@ %a@]" Smtml.Expr.pp e
        in
        Logs.app (fun k -> k "%s" msg);
        Error (`SymFailure msg)
      | _ ->
        let msg =
          Fmt.str "Failure: @[<hov>something went terribly wrong:@ %a@]"
            Smtml.Expr.pp_list result
        in
        Logs.app (fun k -> k "%s" msg);
        Error (`SymFailure msg) )
    | Error err -> (
      match err with
      | `Abort msg -> Ok (Some (`SymAbort msg))
      | `Assert_failure v -> Ok (Some (`SymAssertFailure v))
      | `Exec_failure v -> Ok (Some (`SymExecFailure v))
      | `Eval_failure v -> Ok (Some (`SymEvalFailure v))
      | `ReadFile_failure v -> Ok (Some (`SymReadFileFailure v))
      | `Failure msg -> Result.error (`SymFailure msg) )
  in
  let* witness = process_witness res in
  let+ () = serialize_thread workspace witness thread in
  witness

let err_to_json = function
  | `SymAbort msg -> `Assoc [ ("type", `String "Abort"); ("sink", `String msg) ]
  | `SymAssertFailure v ->
    let v = Value.to_string v in
    `Assoc [ ("type", `String "Assert failure"); ("sink", `String v) ]
  | `SymEvalFailure v ->
    let v = Value.to_string v in
    `Assoc [ ("type", `String "Eval failure"); ("sink", `String v) ]
  | `SymExecFailure v ->
    let v = Value.to_string v in
    `Assoc [ ("type", `String "Exec failure"); ("sink", `String v) ]
  | `SymReadFileFailure v ->
    let v = Value.to_string v in
    `Assoc [ ("type", `String "ReadFile failure"); ("sink", `String v) ]
  | `SymFailure msg ->
    `Assoc [ ("type", `String "Failure"); ("sink", `String msg) ]

let write_report (workspace : Fpath.t) (filename : Fpath.t) (exec_time : float)
  (solver_time : float) (solver_count : int) (problems : witness list) :
  unit Result.t =
  let mode = 0o666 in
  let json =
    `Assoc
      [ ("filename", `String (Fpath.to_string filename))
      ; ("execution_time", `Float exec_time)
      ; ("solver_time", `Float solver_time)
      ; ("solver_queries", `Int solver_count)
      ; ("num_problems", `Int (List.length problems))
      ; ("problems", `List (List.map err_to_json problems))
      ]
  in
  let path = Fpath.(workspace / "symbolic-execution.json") in
  Result.bos
  @@ Bos.OS.File.writef ~mode path "%a" (Yojson.pretty_print ~std:true) json

let no_stop_at_failure = false

let run () (opts : Options.t) : unit Result.t =
  let* p = dispatch_prog opts.lang opts.input in
  let env = link_env opts.input p in
  let start = Stdlib.Sys.time () in
  let thread = Thread.create () in
  let result = Ecma_sl.Symbolic_interpreter.main env opts.target in
  let results = Choice.run result thread in
  let exec_time = Stdlib.Sys.time () -. start in
  let solv_time = !Solver.solver_time in
  let solv_cnt = !Solver.solver_count in
  let testsuite = Fpath.(opts.workspace / "test-suite") in
  let* _ = Result.bos (Bos.OS.Dir.create ~mode:0o777 testsuite) in
  let rec print_and_count_failures (cnt, problems) results =
    match results () with
    | Seq.Nil -> Ok (cnt, problems)
    | Seq.Cons (result, tl) -> (
      let* witness = process_result opts.workspace result in
      match witness with
      | None -> print_and_count_failures (cnt, problems) tl
      | Some witness ->
        let cnt = succ cnt in
        let problems = witness :: problems in
        if no_stop_at_failure then print_and_count_failures (cnt, problems) tl
        else Ok (cnt, problems) )
  in
  let* (n, problems) = print_and_count_failures (0, []) results in
  if n = 0 then Logs.app (fun k -> k "All Ok!")
  else Logs.app (fun k -> k "Found %d problems!" n);
  Logs.debug (fun k -> k "  exec time : %fs" exec_time);
  Logs.debug (fun k -> k "solver time : %fs" solv_time);
  write_report opts.workspace opts.input exec_time solv_time solv_cnt problems
