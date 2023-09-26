open Bos
open Core
module Env = Sym_state.P.Env
module Value = Sym_state.P.Value
module Choice = Sym_state.P.Choice
module Thread = Choice_monad.Thread
module Translator = Value_translator
module Extern_func = Sym_state.P.Extern_func
module SMap = Map.Make (String)

let ( let* ) o f = match o with Error (`Msg e) -> failwith e | Ok v -> f v
let ( let/ ) = Choice.bind

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
    let sub = TriOpt (Operators.Ssubstr, e, i, len) in
    let query = BinOpt (Operators.Eq, sub, Val (Val.Str "; touch success #")) in
    let/ b = Choice.check_add_true query in
    Choice.return (Val (Val.Bool b))
  in
  let is_eval_sat (e : value) =
    (* TODO: more fine-grained exploit analysis *)
    let i = Value.int_symbol_s (fresh "i") in
    let len = Value.int_symbol_s (fresh "len") in
    let sub = TriOpt (Operators.Ssubstr, e, i, len) in
    let query =
      BinOpt (Operators.Eq, sub, Val (Val.Str ";console.log('success')//"))
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
    let symbols = Encoding.Expression.get_symbols [ e' ] in
    let model = Option.value_exn (Batch.model ~symbols solver) in
    match Encoding.Model.evaluate model (List.hd_exn symbols) with
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
  SMap.of_alist_exn
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
    Format.printf "extern print: %s@." (Value.Pp.pp v);
    Choice.return (Value.Val (Val.Symbol "undefined"))
  in
  SMap.of_alist_exn
    [ ("hello", Extern_func (Func (UArg Res), hello))
    ; ("value", Extern_func (Func (Arg Res), print))
    ]

let plus_ext = ".esl"
let core_ext = ".cesl"
let js_ext = ".js"

let dispatch_file_ext on_plus on_core on_js file =
  if Filename.check_suffix file plus_ext then on_plus file
  else if Filename.check_suffix file core_ext then on_core file
  else if Filename.check_suffix file js_ext then on_js file
  else raise (Sys_error (file ^ " :unreconized file type"))

let prog_of_plus file =
  let e_prog =
    Parsing_utils.(
      apply_prog_macros
        (resolve_prog_imports (parse_e_prog file (load_file file))) )
  in
  Compiler.compile_prog e_prog

let prog_of_core file = Parsing_utils.(parse_prog (load_file file))

let js2ecma_sl file output =
  Cmd.(v "js2ecma-sl" % "-c" % "-i" % file % "-o" % output)

let prog_of_js interp file =
  let* exists_file = OS.File.exists (Fpath.v file) in
  assert exists_file;
  let ast_file = Filename.chop_extension file in
  let* () = OS.Cmd.run (js2ecma_sl file ast_file) in
  let ast_str = In_channel.read_all ast_file in
  let interp = In_channel.read_all interp in
  let program = String.concat ~sep:";\n" [ ast_str; interp ] in
  let* () = OS.File.delete (Fpath.v ast_file) in
  Parsing_utils.parse_prog program

let link_env prog =
  let env = Env.Build.empty () in
  let env = Env.Build.add_functions env prog in
  let env = Env.Build.add_extern_functions env extern_functions in
  Env.Build.add_extern_functions env symbolic_api_funcs

let _error at category msg =
  Format.eprintf "%s:%s:%s@." (Source.string_of_region at) category msg

let serialize =
  let counter = ref 0 in
  fun ?(witness : string option) thread ->
    let pc = Thread.pc thread in
    let solver = Thread.solver thread in
    assert (Batch.check solver pc);
    let model = Batch.model solver in
    let testcase =
      Option.value_map model ~default:"" ~f:(fun m ->
        let open Encoding in
        let inputs =
          List.map (Model.get_bindings m) ~f:(fun (s, v) ->
            let _sort = Types.string_of_type (Symbol.type_of s) in
            let name = Symbol.to_string s in
            let interp = Value.to_string v in
            sprintf "\"%s\" : %s" name interp )
        in
        "module.exports.symbolic_map = \n  { "
        ^ String.concat ~sep:"\n  , " inputs
        ^ "\n  }" )
    in
    let str_pc = Encoding.Expression.string_of_pc pc in
    let smt_query = Encoding.Expression.to_smt pc in
    let prefix =
      incr counter;
      let fname = if Option.is_some witness then "witness" else "testecase" in
      let fname = sprintf "%s-%i" fname !counter in
      Filename.concat (Filename.concat !Config.workspace "test-suite") fname
    in
    Io.write_file ~file:(sprintf "%s.js" prefix) ~data:testcase;
    Io.write_file ~file:(sprintf "%s.pc" prefix) ~data:str_pc;
    Io.write_file ~file:(sprintf "%s.smt2" prefix) ~data:smt_query;
    Option.iter witness ~f:(fun sink ->
      Io.write_file ~file:(sprintf "%s_sink.json" prefix) ~data:sink )

let run env entry_func =
  let testsuite_path = Filename.concat !Config.workspace "test-suite" in
  Io.safe_mkdir testsuite_path;
  let start = Stdlib.Sys.time () in
  let thread = Choice_monad.Thread.create () in
  let result = Eval.S.main env entry_func in
  let results = Choice.run result thread in
  List.iter results ~f:(fun (ret, thread) ->
    let witness = match ret with Ok _ -> None | Error err -> Some err in
    serialize ?witness thread;
    let pc = Encoding.Expression.string_of_pc @@ Thread.pc thread in
    Format.printf "  path cond : %s@." pc );
  Format.printf "  exec time : %fs@." (Stdlib.Sys.time () -. start);
  Format.printf "solver time : %fs@." !Batch.solver_time;
  Format.printf "  mean time : %fms@."
    (1000. *. !Batch.solver_time /. float !Batch.solver_count)

let main target workspace interpreter debug file =
  Config.target := target;
  Config.workspace := workspace;
  Log.on_debug := debug;
  Config.file := file;
  let prog =
    dispatch_file_ext prog_of_plus prog_of_core (prog_of_js interpreter) file
  in
  let env = link_env prog in
  run env target

let file =
  let doc = "analysis files" in
  Cmdliner.Arg.(required & pos 0 (some file) None & info [] ~doc)

let target =
  let doc = "target function to analyse" in
  Cmdliner.Arg.(value & opt string "main" & info [ "target"; "d" ] ~doc)

let workspace =
  let doc = "write result file to directory" in
  Cmdliner.Arg.(value & opt string "output" & info [ "workspace"; "o" ] ~doc)

let interpreter =
  let doc = "path to ECMAscript interpreter" in
  Cmdliner.Arg.(value & opt string "es6.cesl" & info [ "interp" ] ~doc)

let debug =
  let doc = "debug mode" in
  Cmdliner.Arg.(value & flag & info [ "debug" ] ~doc)

let cli =
  let open Cmdliner in
  let doc = "ECMA-SL symbolic analysis" in
  let man = [ `S Manpage.s_bugs ] in
  let info = Cmd.info "ecma-se" ~version:"%%VERSION%%" ~doc ~man in
  Cmd.v info Term.(const main $ target $ workspace $ interpreter $ debug $ file)

let () =
  Backtrace.Exn.set_recording true;
  try exit (Cmdliner.Cmd.eval cli)
  with exn ->
    Stdlib.flush_all ();
    Printexc.print_backtrace stdout;
    Format.eprintf "%s: uncaught exception %s@."
      (Sys.get_argv ()).(0)
      (Exn.to_string exn);
    exit 2
