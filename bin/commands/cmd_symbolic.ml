open Bos_setup
open Ecma_sl
open Syntax.Result
module Env = Symbolic.P.Env
module Value = Symbolic.P.Value
module Choice = Symbolic.P.Choice
module Thread = Choice_monad.Thread
module Translator = Value_translator
module Extern_func = Symbolic.P.Extern_func

let print_time = false
let print_pc = false

let list_iter ~f lst =
  let exception E of Rresult.R.msg in
  try
    List.iter
      (fun v -> match f v with Error s -> raise (E s) | Ok () -> ())
      lst;
    Ok ()
  with E s -> Error s

let plus_ext = ".esl"
let core_ext = ".cesl"
let js_ext = ".js"

let dispatch_file_ext on_plus on_core on_js (file : Fpath.t) =
  if Fpath.has_ext plus_ext file then Ok (on_plus file)
  else if Fpath.has_ext core_ext file then Ok (on_core file)
  else if Fpath.has_ext js_ext file then on_js file
  else Error (`Msg (Format.asprintf "%a :unreconized file type" Fpath.pp file))

let prog_of_plus file =
  let open Parsing_utils in
  let file = Fpath.to_string file in
  load_file file
  |> parse_eprog ~file
  |> resolve_eprog_imports
  |> apply_eprog_macros
  |> Compiler.compile_prog

let prog_of_core file =
  let file = Fpath.to_string file in
  Parsing_utils.load_file file |> Parsing_utils.parse_prog ~file

let js2ecma_sl file output =
  Cmd.(v "js2ecma-sl" % "-c" % "-i" % p file % "-o" % p output)

let prog_of_js file =
  let ast_file = Fpath.(file -+ "_ast.cesl") in
  let* () = OS.Cmd.run (js2ecma_sl file ast_file) in
  let* ast = OS.File.read ast_file in
  let* es6 = OS.File.read (Fpath.v (Option.get (Share.get_es6 ()))) in
  let program = String.concat ~sep:";\n" [ ast; es6 ] in
  let* () = OS.File.delete ast_file in
  Ok (Parsing_utils.parse_prog program)

let link_env prog =
  Env.Build.empty ()
  |> Env.Build.add_functions prog
  |> Env.Build.add_extern_functions Symbolic_extern.api

let pp_model fmt v =
  let open Encoding in
  let pp_mapping fmt (s, v) =
    Fmt.fprintf fmt {|"%a" : %a|} Symbol.pp s Value.pp v
  in
  let pp_vars fmt v =
    Fmt.pp_print_list
      ~pp_sep:(fun fmt () -> Fmt.fprintf fmt "@\n, ")
      pp_mapping fmt v
  in
  Fmt.fprintf fmt "@[<v 2>module.exports.symbolic_map =@ { %a@\n}@]" pp_vars
    (Model.get_bindings v)

let serialize ~workspace =
  let (next_int, _) = Utils.make_counter 0 1 in
  fun ?(witness : string option) thread ->
    let module Term = Encoding.Expr in
    let pc = Thread.pc thread in
    let solver = Thread.solver thread in
    assert (Solver.check solver pc);
    let m = Solver.model solver in
    let f =
      Fmt.ksprintf
        Fpath.(add_seg (workspace / "test-suite"))
        (match witness with None -> "testcase-%d" | Some _ -> "witness-%d")
        (next_int ())
    in
    let* () = OS.File.writef Fpath.(f + ".js") "%a" (Fmt.pp_opt pp_model) m in
    let* () = OS.File.writef Fpath.(f + ".pc") "%a" Term.pp_list pc in
    let* () = OS.File.writef Fpath.(f + ".smtml") "%a" Term.pp_smt pc in
    match witness with
    | None -> Ok ()
    | Some witness -> OS.File.writef Fpath.(f + "_sink.json") "%s" witness

let run ~workspace env entry_func =
  let start = Stdlib.Sys.time () in
  let thread = Choice_monad.Thread.create () in
  let result = Symbolic_interpreter.main env entry_func in
  let results = Choice.run result thread in
  let testsuite_path = Fpath.(workspace / "test-suite") in
  ( match OS.Dir.create ~path:true testsuite_path with
  | Ok _ -> ()
  | Error (`Msg s) -> Log.err "%s" s );
  List.iter
    (fun (ret, thread) ->
      let witness = match ret with Ok _ -> None | Error err -> Some err in
      Log.on_err Fun.id @@ serialize ~workspace ?witness thread;
      if print_pc then
        Log.app "  path cond : %a@." Encoding.Expr.pp_list (Thread.pc thread) )
    results;
  if print_time then (
    Log.app "  exec time : %fs@." (Stdlib.Sys.time () -. start);
    Log.app "solver time : %fs@." !Solver.solver_time;
    Log.app "  mean time : %fms@."
      (1000. *. !Solver.solver_time /. float !Solver.solver_count) )

let main (copts : Options.common_options) (file : Fpath.t) (target : string)
  (workspace : Fpath.t) =
  Log.on_debug := copts.debug;
  match dispatch_file_ext prog_of_plus prog_of_core prog_of_js file with
  | Error (`Msg s) ->
    Log.warn "%s" s;
    1
  | Ok prog ->
    let env = link_env prog in
    run ~workspace env target;
    0

let node test witness = Cmd.(v "node" % p test % p witness)

type observable =
  | Stdout of string
  | File of string

let observable_effects = [ Stdout "success"; File "success" ]

let execute_witness env (test : Fpath.t) (witness : Fpath.t) =
  let open OS in
  Log.app " running : %s" @@ Fpath.to_string witness;
  let cmd = node test witness in
  let* (out, status) = Cmd.(run_out ~env ~err:err_run_out cmd |> out_string) in
  match status with
  | (_, `Exited 0) ->
    Ok
      (List.find_opt
         (fun effect ->
           match effect with
           | Stdout sub -> String.find_sub ~sub out |> Option.is_some
           | File file -> Sys.file_exists file )
         observable_effects )
  | _ -> Error (`Msg (Fmt.sprintf "unexpected node failure: %s" out))

let validate (copts : Options.common_options) filename suite_path =
  if copts.debug then Logs.set_level (Some Logs.Debug);
  Log.app "validating : %a..." Fpath.pp filename;
  let node_loc = List.nth Share.nodejs_location 0 in
  let node_path = Fmt.sprintf ".:%s" node_loc in
  let env = String.Map.of_list [ ("NODE_PATH", node_path) ] in
  (let* witnesses = OS.Path.matches Fpath.(suite_path / "witness-$(n).js") in
   let* () =
     list_iter witnesses ~f:(fun witness ->
         let* effect = execute_witness env filename witness in
         match effect with
         | Some (Stdout msg) ->
           Log.app " status : true (\"%s\" in output)" msg;
           Ok ()
         | Some (File file) ->
           let* () = OS.Path.delete @@ Fpath.v file in
           Log.app " status : true (created file \"%s\")" file;
           Ok ()
         | None ->
           Log.app " status : false (no side effect)";
           Ok () )
   in
   Ok 0 )
  |> Log.on_err (fun () -> 1)
