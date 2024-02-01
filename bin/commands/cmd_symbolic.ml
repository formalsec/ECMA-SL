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

let dispatch_file_ext on_plus on_core on_js file =
  if Filename.check_suffix file plus_ext then Ok (on_plus file)
  else if Filename.check_suffix file core_ext then Ok (on_core file)
  else if Filename.check_suffix file js_ext then on_js file
  else Error (`Msg (file ^ " :unreconized file type"))

let prog_of_plus file =
  let open Parsing_utils in
  load_file file
  |> parse_eprog ~file
  |> resolve_eprog_imports
  |> apply_eprog_macros
  |> Compiler.compile_prog

let prog_of_core file =
  Parsing_utils.load_file file |> Parsing_utils.parse_prog ~file

let js2ecma_sl file output =
  Cmd.(v "js2ecma-sl" % "-c" % "-i" % p file % "-o" % p output)

let prog_of_js file =
  let* file = OS.File.must_exist (Fpath.v file) in
  let ast_file = Fpath.(file -+ "_ast.cesl") in
  let* () = OS.Cmd.run (js2ecma_sl file ast_file) in
  let ast_chan = open_in @@ Fpath.to_string ast_file in
  let interp_chan = open_in (Option.get (Share.get_es6_sym ())) in
  Fun.protect
    ~finally:(fun () ->
      close_in ast_chan;
      close_in interp_chan )
    (fun () ->
      let ast_str = In_channel.input_all ast_chan in
      let interp = In_channel.input_all interp_chan in
      let program = String.concat ~sep:";\n" [ ast_str; interp ] in
      let* () = OS.File.delete ast_file in
      Ok (Parsing_utils.parse_prog program) )

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

let serialize =
  let (next_int, _) = Utils.make_counter 0 1 in
  fun ?(witness : string option) thread ->
    let pc = Thread.pc thread in
    let solver = Thread.solver thread in
    assert (Solver.check solver pc);
    let model = Solver.model solver in
    let testcase =
      Option.fold model ~none:"" ~some:(Fmt.asprintf "%a" pp_model)
    in
    let str_pc = Fmt.asprintf "%a" Encoding.Expr.pp_list pc in
    let smt_query = Fmt.asprintf "%a" Encoding.Expr.pp_smt pc in
    let prefix =
      let fname = if Option.is_some witness then "witness" else "testecase" in
      let fname = Fmt.sprintf "%s-%i" fname (next_int ()) in
      Filename.concat (Filename.concat !Config.workspace "test-suite") fname
    in
    Io.write_file (Fmt.sprintf "%s.js" prefix) testcase;
    Io.write_file (Fmt.sprintf "%s.pc" prefix) str_pc;
    Io.write_file (Fmt.sprintf "%s.smt2" prefix) smt_query;
    Option.iter
      (fun sink -> Io.write_file (Fmt.sprintf "%s_sink.json" prefix) sink)
      witness

let run env entry_func =
  let testsuite_path = Filename.concat !Config.workspace "test-suite" in
  Io.safe_mkdir testsuite_path;
  let start = Stdlib.Sys.time () in
  let thread = Choice_monad.Thread.create () in
  let result = Symbolic_interpreter.main env entry_func in
  let results = Choice.run result thread in
  List.iter
    (fun (ret, thread) ->
      let witness = match ret with Ok _ -> None | Error err -> Some err in
      serialize ?witness thread;
      if print_pc then
        Fmt.printf "  path cond : %a@." Encoding.Expr.pp_list (Thread.pc thread)
      )
    results;
  if print_time then (
    Fmt.printf "  exec time : %fs@." (Stdlib.Sys.time () -. start);
    Fmt.printf "solver time : %fs@." !Solver.solver_time;
    Fmt.printf "  mean time : %fms@."
      (1000. *. !Solver.solver_time /. float !Solver.solver_count) )

let main (copts : Options.Common.t) file target workspace =
  Options.Common.set copts;
  Config.workspace := workspace;
  (let* prog = dispatch_file_ext prog_of_plus prog_of_core prog_of_js file in
   let env = link_env prog in
   run env target;
   Ok 0 )
  |> Logs.on_error_msg ~use:(fun () -> 1)

let node test witness = Cmd.(v "node" % test % p witness)

type observable =
  | Stdout of string
  | File of string

let observable_effects = [ Stdout "success"; File "success" ]

let execute_witness env (test : string) (witness : Fpath.t) =
  let open OS in
  Logs.app (fun m -> m " running : %s" @@ Fpath.to_string witness);
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

let validate (copts : Options.Common.t) filename suite_path =
  if copts.debug then Logs.set_level (Some Logs.Debug);
  Logs.app (fun m -> m "validating : %s..." filename);
  let node_loc = List.nth Share.nodejs_location 0 in
  let node_path = Fmt.sprintf ".:%s" node_loc in
  let env = String.Map.of_list [ ("NODE_PATH", node_path) ] in
  (let* witnesses = OS.Path.matches Fpath.(v suite_path / "witness-$(n).js") in
   let* () =
     list_iter witnesses ~f:(fun witness ->
         let* effect = execute_witness env filename witness in
         match effect with
         | Some (Stdout msg) ->
           Logs.app (fun m -> m " status : true (\"%s\" in output)" msg);
           Ok ()
         | Some (File file) ->
           let* () = OS.Path.delete @@ Fpath.v file in
           Logs.app (fun m -> m " status : true (created file \"%s\")" file);
           Ok ()
         | None ->
           Logs.app (fun m -> m " status : false (no side effect)");
           Ok () )
   in
   Ok 0 )
  |> Logs.on_error_msg ~use:(fun () -> 1)
