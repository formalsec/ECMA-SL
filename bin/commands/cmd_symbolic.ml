open Bos_setup
open Ecma_sl
open Syntax.Result
module Env = Sym_state.P.Env
module Value = Sym_state.P.Value
module Choice = Sym_state.P.Choice
module Thread = Choice_monad.Thread
module Translator = Value_translator
module Extern_func = Sym_state.P.Extern_func
module SMap = Stdlib.Map.Make (Stdlib.String)

let ( let/ ) = Choice.bind

let list_iter ~f lst =
  let exception E of Rresult.R.msg in
  try
    List.iter
      (fun v -> match f v with Error s -> raise (E s) | Ok () -> ())
      lst;
    Ok ()
  with E s -> Error s

let fresh : string -> string =
  let counter = ref (-1) in
  fun x ->
    incr counter;
    Format.sprintf "%s_%d" x !counter

let symbolic_api_funcs =
  let open Value in
  let open Extern_func in
  let str_symbol (x : value) = Choice.return (Symbolic (Type.StrType, x)) in
  let int_symbol (x : value) = Choice.return (Value.int_symbol x) in
  let flt_symbol (x : value) = Choice.return (Symbolic (Type.FltType, x)) in
  let bool_symbol (x : value) = Choice.return (Symbolic (Type.BoolType, x)) in
  let is_symbolic (n : value) =
    Choice.return (Val (Val.Bool (Value.is_symbolic n)))
  in
  let is_number (n : value) =
    let is_number =
      match Value_typing.type_of n with
      | Some Type.IntType | Some Type.FltType -> true
      | _ -> false
    in
    Choice.return (Val (Val.Bool is_number))
  in
  let is_sat (e : value) =
    let/ b = Choice.check e in
    Choice.return (Val (Val.Bool b))
  in
  let is_exec_sat (e : value) =
    (* TODO: more fine-grained exploit analysis *)
    let i = Value.int_symbol_s (fresh "i") in
    let len = Value.int_symbol_s (fresh "len") in
    let sub = TriOpt (Operator.StringSubstr, e, i, len) in
    let query = BinOpt (Operator.Eq, sub, Val (Val.Str "; touch success #")) in
    let/ b = Choice.check_add_true query in
    Choice.return (Val (Val.Bool b))
  in
  let is_eval_sat (e : value) =
    (* TODO: more fine-grained exploit analysis *)
    let i = Value.int_symbol_s (fresh "i") in
    let len = Value.int_symbol_s (fresh "len") in
    let sub = TriOpt (Operator.StringSubstr, e, i, len) in
    let query =
      BinOpt (Operator.Eq, sub, Val (Val.Str ";console.log('success')//"))
    in
    let/ b = Choice.check_add_true query in
    Choice.return (Val (Val.Bool b))
  in
  let assume (e : value) thread =
    let e' = Translator.translate e in
    [ (Val (Val.Symbol "undefined"), Thread.add_pc thread e') ]
  in
  let evaluate (e : value) thread =
    let e' = Translator.translate e in
    let pc = Thread.pc thread in
    let solver = Thread.solver thread in
    assert (Batch.check solver (e' :: pc));
    let symbols = Encoding.Expr.get_symbols [ e' ] in
    let model = Option.get (Batch.model ~symbols solver) in
    match Encoding.Model.evaluate model (List.hd symbols) with
    | Some v -> [ (Translator.expr_of_value v, thread) ]
    | None -> assert false (* Should never happpen due to sat check above *)
  in
  let maximize (e : value) thread =
    let e' = Translator.translate e in
    let pc = Thread.pc thread in
    let opt = Thread.optimizer thread in
    let v = Encoding.Optimizer.maximize opt e' pc in
    match v with
    | Some v -> [ (Translator.expr_of_value v, thread) ]
    | None -> assert false
  in
  let minimize (e : value) thread =
    let e' = Translator.translate e in
    let pc = Thread.pc thread in
    let opt = Thread.optimizer thread in
    let v = Encoding.Optimizer.minimize opt e' pc in
    match v with
    | Some v -> [ (Translator.expr_of_value v, thread) ]
    | None -> assert false
  in
  SMap.of_list
    [ ("str_symbol", Extern_func (Func (Arg Res), str_symbol))
    ; ("int_symbol", Extern_func (Func (Arg Res), int_symbol))
    ; ("flt_symbol", Extern_func (Func (Arg Res), flt_symbol))
    ; ("bool_symbol", Extern_func (Func (Arg Res), bool_symbol))
    ; ("is_symbolic", Extern_func (Func (Arg Res), is_symbolic))
    ; ("is_number", Extern_func (Func (Arg Res), is_number))
    ; ("is_sat", Extern_func (Func (Arg Res), is_sat))
    ; ("is_exec_sat", Extern_func (Func (Arg Res), is_exec_sat))
    ; ("is_eval_sat", Extern_func (Func (Arg Res), is_eval_sat))
    ; ("assume", Extern_func (Func (Arg Res), assume))
    ; ("evaluate", Extern_func (Func (Arg Res), evaluate))
    ; ("maximize", Extern_func (Func (Arg Res), maximize))
    ; ("minimize", Extern_func (Func (Arg Res), minimize))
    ]

(* Examples *)
let extern_functions =
  let open Extern_func in
  let hello () =
    Format.printf "Hello world@.";
    Choice.return (Value.Val (Val.Symbol "undefined"))
  in
  let print (v : Value.value) =
    Format.printf "extern print: %a@." Value.Pp.pp v;
    Choice.return (Value.Val (Val.Symbol "undefined"))
  in
  SMap.of_list
    [ ("hello", Extern_func (Func (UArg Res), hello))
    ; ("value", Extern_func (Func (Arg Res), print))
    ]

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
  Io.load_file file
  |> parse_e_prog file
  |> resolve_prog_imports
  |> apply_prog_macros
  |> Compiler.compile_prog

let prog_of_core file = Io.load_file file |> Parsing_utils.parse_prog

let js2ecma_sl file output =
  Cmd.(v "js2ecma-sl" % "-c" % "-i" % p file % "-o" % p output)

let prog_of_js file =
  let* file = OS.File.must_exist (Fpath.v file) in
  let ast_file = Fpath.(file -+ "_ast.cesl") in
  let* () = OS.Cmd.run (js2ecma_sl file ast_file) in
  let ast_chan = open_in @@ Fpath.to_string ast_file in
  let interp_chan = open_in (Option.get (Esl_share.get_es6 ())) in
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
  |> Env.Build.add_extern_functions extern_functions
  |> Env.Build.add_extern_functions symbolic_api_funcs

let serialize =
  let counter = ref 0 in
  fun ?(witness : string option) thread ->
    let pc = Thread.pc thread in
    let solver = Thread.solver thread in
    assert (Batch.check solver pc);
    let model = Batch.model solver in
    let testcase =
      Option.fold model ~none:"" ~some:(fun m ->
          let open Encoding in
          Format.asprintf "module.exports.symbolic_map = @[<h 2>{%a@\n}@]"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
               (fun fmt (s, v) ->
                 Format.fprintf fmt {|"%a" : %a|} Symbol.pp s Value.pp v ) )
            (Model.get_bindings m) )
    in
    let str_pc = Format.asprintf "%a" Encoding.Expr.pp_list pc in
    let smt_query = Format.asprintf "%a" Encoding.Expr.pp_smt pc in
    let prefix =
      incr counter;
      let fname = if Option.is_some witness then "witness" else "testecase" in
      let fname = Format.sprintf "%s-%i" fname !counter in
      Filename.concat (Filename.concat !Config.workspace "test-suite") fname
    in
    Io.write_file ~file:(Format.sprintf "%s.js" prefix) ~data:testcase;
    Io.write_file ~file:(Format.sprintf "%s.pc" prefix) ~data:str_pc;
    Io.write_file ~file:(Format.sprintf "%s.smt2" prefix) ~data:smt_query;
    Option.iter
      (fun sink ->
        Io.write_file ~file:(Format.sprintf "%s_sink.json" prefix) ~data:sink )
      witness

let run env entry_func =
  let testsuite_path = Filename.concat !Config.workspace "test-suite" in
  Io.safe_mkdir testsuite_path;
  let start = Stdlib.Sys.time () in
  let thread = Choice_monad.Thread.create () in
  let result = Eval.S.main env entry_func in
  let results = Choice.run result thread in
  List.iter
    (fun (ret, thread) ->
      let witness = match ret with Ok _ -> None | Error err -> Some err in
      serialize ?witness thread;
      Format.printf "  path cond : %a@." Encoding.Expr.pp_list
        (Thread.pc thread) )
    results;
  Format.printf "  exec time : %fs@." (Stdlib.Sys.time () -. start);
  Format.printf "solver time : %fs@." !Batch.solver_time;
  Format.printf "  mean time : %fms@."
    (1000. *. !Batch.solver_time /. float !Batch.solver_count)

let main (copts : Options.common_options) file target workspace =
  Log.on_debug := copts.debug;
  Config.file := file;
  Config.target := target;
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
  | _ -> Error (`Msg (Format.sprintf "unexpected node failure: %s" out))

let validate (copts : Options.common_options) filename suite_path =
  if copts.debug then Logs.set_level (Some Logs.Debug);
  Logs.app (fun m -> m "validating : %s..." filename);
  let node_loc = List.nth Esl_share.nodejs_location 0 in
  let node_path = Printf.sprintf ".:%s" node_loc in
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
