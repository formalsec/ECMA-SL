open Core
module Env = Sym_state.P.Env
module Value = Sym_state.P.Value
module Choice = Sym_state.P.Choice
module Thread = Choice_monad.Thread
module SMap = Map.Make (String)

let symbolic_api_funcs =
  let open Value in
  let open Sym_state.P.Extern_func in
  let str_symbol (x : value) : value = Symbolic (Type.StrType, x) in
  let int_symbol (x : value) : value = Symbolic (Type.IntType, x) in
  let flt_symbol (x : value) : value = Symbolic (Type.FltType, x) in
  let bool_symbol (x : value) : value = Symbolic (Type.BoolType, x) in
  let is_symbolic (n : value) : value = Val (Val.Bool (Value.is_symbolic n)) in
  let is_number (n : value) : value =
    let is_number =
      match Value_typing.type_of n with
      | Some Type.IntType | Some Type.FltType -> true
      | _ -> false
    in
    Val (Val.Bool is_number)
  in
  let is_sat (_e : value) : value = assert false in
  let assume (_e : value) : value = assert false in
  let evaluate (_e : value) : value = assert false in
  let maximize (_e : value) : value = assert false in
  let minimize (_e : value) : value = assert false in
  SMap.of_alist_exn
    [ ("str_symbol", Extern_func (Func (Arg Res), str_symbol))
    ; ("int_symbol", Extern_func (Func (Arg Res), int_symbol))
    ; ("flt_symbol", Extern_func (Func (Arg Res), flt_symbol))
    ; ("bool_symbol", Extern_func (Func (Arg Res), bool_symbol))
    ; ("is_symbolic", Extern_func (Func (Arg Res), is_symbolic))
    ; ("is_number", Extern_func (Func (Arg Res), is_number))
    ; ("is_sat", Extern_func (Func (Arg Res), is_sat))
    ; ("assume", Extern_func (Func (Arg Res), assume))
    ; ("evaluate", Extern_func (Func (Arg Res), evaluate))
    ; ("maximize", Extern_func (Func (Arg Res), maximize))
    ; ("minimize", Extern_func (Func (Arg Res), minimize))
    ]

(* Examples *)
let extern_functions =
  let open Sym_state.P.Extern_func in
  let hello () : Value.value =
    Format.printf "Hello world@.";
    Value.Val (Val.Symbol "undefined")
  in
  let print (v : Value.value) : Value.value =
    Format.printf "extern print: %s@." (Value.Pp.pp v);
    Value.Val (Val.Symbol "undefined")
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

let prog_of_js interp file =
  assert (Sys_unix.file_exists_exn interp);
  let ast_file = Filename.chop_extension file in
  let ret =
    Sys_unix.command
      (String.concat ~sep:" "
         [ "js2ecma-sl"; "-c"; "-i"; file; "-o"; ast_file ] )
  in
  if ret <> 0 then raise (Sys_error ("unable to compile: " ^ file))
  else
    let ast_str = In_channel.read_all ast_file in
    let interp = In_channel.read_all interp in
    let program = String.concat ~sep:";\n" [ ast_str; interp ] in
    Sys_unix.remove ast_file;
    Parsing_utils.parse_prog program

let link_env prog =
  let env = Env.Build.empty () in
  let env = Env.Build.add_functions env prog in
  let env = Env.Build.add_extern_functions env extern_functions in
  Env.Build.add_extern_functions env symbolic_api_funcs

let _error at category msg =
  Format.eprintf "%s:%s:%s@." (Source.string_of_region at) category msg

let run env target =
  let start = Stdlib.Sys.time () in
  let thread = Choice_monad.Thread.create () in
  let result = Eval.S.main env target in
  let results = Choice.run result thread in
  List.iter results ~f:(fun (_, thread) ->
    let pc = Encoding.Expression.string_of_pc @@ Thread.pc thread in
    Format.printf "  path cond : %s@." pc );
  Format.printf "  exec time : %fs@." (Stdlib.Sys.time () -. start);
  Format.printf "solver time : %fs@." !Batch.solver_time;
  Format.printf "  mean time : %fms@."
    (1000. *. !Batch.solver_time /. float !Batch.solver_count)

let command_parameters : (unit -> unit) Command.Param.t =
  let%map_open.Command files =
    anon (sequence ("filename" %: Filename_unix.arg_type))
  and target =
    flag "target" ~aliases:[ "d" ]
      (optional_with_default "main" string)
      ~doc:"string target function to analyse"
  and workspace =
    flag "workspace" ~aliases:[ "o" ]
      (optional_with_default "output" string)
      ~doc:"string write result files to directory"
  and policy =
    flag "policy"
      (optional_with_default "breadth" string)
      ~doc:"string search policy (depth|breadth|random)"
  and interp =
    flag "interp"
      (optional_with_default "es6.cesl" string)
      ~doc:"path to ECMAScript interpreter"
  and debug = flag "debug" no_arg ~doc:" verbose interpreter" in
  fun () ->
    Config.target := target;
    Config.workspace := workspace;
    Config.policy := policy;
    Log.on_debug := debug;
    List.iter files ~f:(fun f ->
      Config.file := f;
      let prog =
        dispatch_file_ext prog_of_plus prog_of_core (prog_of_js interp) f
      in
      let env = link_env prog in
      run env target )

let command =
  Command.basic ~summary:"ECMA-SL symbolic analysis" command_parameters

let () =
  Backtrace.Exn.set_recording true;
  try Command_unix.run ~version:"0.1.0" command
  with exn ->
    Caml.flush_all ();
    Printexc.print_backtrace stdout;
    Format.eprintf "%s: uncaught exception %s@."
      (Sys.get_argv ()).(0)
      (Exn.to_string exn);
    exit 2
