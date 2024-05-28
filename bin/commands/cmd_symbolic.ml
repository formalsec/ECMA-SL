open Ecma_sl
open Ecma_sl.Syntax.Result
module PC = Choice_monad.PC
module Thread = Choice_monad.Thread
module Env = Symbolic.P.Env
module Value = Symbolic.P.Value
module Choice = Symbolic.P.Choice
module Extern_func = Symbolic.P.Extern_func
module Translator = Value_translator
module State = Symbolic_interpreter.State

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

let prog_of_js (fpath : Fpath.t) : Prog.t Result.t =
  let interp = Share.es6_sym_interp () |> Parsing.parse_prog in
  (* We can use interpreter will all the metadata once the value_translator can handle nary operators *)
  (* let instrument = Cmd_interpret.Options.default_instrument () in *)
  (* let* (interp, _) = Cmd_execute.setup_execution ECMARef6Sym None instrument in *)
  let ast = Fpath.v (Filename.temp_file "ecmasl" "ast.cesl") in
  let* () = Cmd_encode.encode None fpath (Some ast) in
  let* build_ast = Cmd_execute.build_ast ast in
  Hashtbl.replace (Prog.funcs interp) (Func.name' build_ast) build_ast;
  Ok interp

let dispatch_prog (lang : Enums.Lang.t) (fpath : Fpath.t) : Prog.t Result.t =
  let valid_langs = Enums.Lang.valid_langs Options.langs lang in
  match Enums.Lang.resolve_file_lang valid_langs fpath with
  | Some CESL -> Cmd_compile.load fpath
  | Some ESL -> Cmd_compile.compile true fpath
  | Some JS -> prog_of_js fpath
  | _ ->
    let msg = Fmt.str "%a :unreconized file type" Fpath.pp fpath in
    Result.error (`Generic msg)

let link_env (prog : Prog.t) : Extern_func.extern_func Symbolic.Env.t =
  let env0 = Env.Build.empty () |> Env.Build.add_functions prog in
  Env.Build.add_extern_functions (Symbolic_extern.extern_cmds env0) env0
  |> Env.Build.add_extern_functions Symbolic_extern.concrete_api
  |> Env.Build.add_extern_functions Symbolic_extern.symbolic_api

let pp_model (ppf : Fmt.t) (model : Encoding.Model.t) : unit =
  let open Fmt in
  let open Encoding in
  let pp_map ppf (s, v) = format ppf {|"%a" : %a|} Symbol.pp s Value.pp v in
  let pp_vars ppf v = pp_lst !>"@\n, " pp_map ppf v in
  format ppf "@[<v 2>module.exports.symbolic_map =@ { %a@\n}@]" pp_vars
    (Model.get_bindings model)

type witness =
  [ `SymAbort of string
  | `SymAssertFailure of Extern_func.value
  ]

let serialize_thread (workspace : Fpath.t) :
  witness option -> Thread.t -> unit Result.t =
  let module Term = Encoding.Expr in
  let (next, _) = Base.make_counter 0 1 in
  fun witness thread ->
    let open Fpath in
    let pc = PC.to_list @@ Thread.pc thread in
    Log.debug "  path cond : %a@." Term.pp_list pc;
    let solver = Thread.solver thread in
    assert (Solver.check solver pc);
    let model = Solver.model solver in
    let path =
      Fmt.ksprintf
        (add_seg (workspace / "test-suite"))
        (match witness with None -> "testcase-%d" | Some _ -> "witness-%d")
        (next ())
    in
    let pp = Fmt.pp_opt pp_model in
    let* () = Result.bos (Bos.OS.File.writef (path + ".js") "%a" pp model) in
    Result.bos (Bos.OS.File.writef (path + ".smtml") "%a" Term.pp_smt pc)

let process_result (workspace : Fpath.t)
  ((res, thread) : State.return_result * Thread.t) : witness option Result.t =
  let process_witness = function
    | Ok _ -> Ok None
    | Error (`Abort msg) -> Ok (Some (`SymAbort msg))
    | Error (`Assert_failure v) -> Ok (Some (`SymAssertFailure v))
    | Error (`Failure msg) -> Result.error (`SymFailure msg)
  in
  let* witness = process_witness res in
  let* () = serialize_thread workspace witness thread in
  Ok witness

let err_to_json = function
  | `SymAbort msg -> `Assoc [ ("type", `String "Abort"); ("sink", `String msg) ]
  | `SymAssertFailure v ->
    let v = Fmt.str "%a" Value.pp v in
    `Assoc [ ("type", `String "Assert failure"); ("sink", `String v) ]
  | `SymFailure msg ->
    `Assoc [ ("type", `String "Failure"); ("sink", `String msg) ]

let write_report (workspace : Fpath.t) (filename : Fpath.t) (exec_time : float)
  (solver_time : float) (solver_count : int) (problems : witness list) :
  unit Result.t =
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
  Result.bos (Bos.OS.File.writef path "%a" (Yojson.pretty_print ~std:true) json)

let run () (opts : Options.t) : unit Result.t =
  let* p = dispatch_prog opts.lang opts.input in
  let env = link_env p in
  let start = Stdlib.Sys.time () in
  let thread = Choice_monad.Thread.create () in
  let result = Symbolic_interpreter.main env opts.target in
  let results = Choice.run result thread in
  let exec_time = Stdlib.Sys.time () -. start in
  let solv_time = !Solver.solver_time in
  let solv_cnt = !Solver.solver_count in
  let testsuite = Fpath.(opts.workspace / "test-suite") in
  let* _ = Result.bos (Bos.OS.Dir.create testsuite) in
  let* problems = list_filter_map ~f:(process_result opts.workspace) results in
  let nproblems = List.length problems in
  if nproblems = 0 then Log.stdout "All Ok!@."
  else Log.stdout "Found %d problems!@." nproblems;
  Log.debug "  exec time : %fs@." exec_time;
  Log.debug "solver time : %fs@." solv_time;
  write_report opts.workspace opts.input exec_time solv_time solv_cnt problems
