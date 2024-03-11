open EslCore
open Ecma_sl

type error =
  | Failure
  | Error
  | CompileError
  | RuntimeError

exception Command_error of error

let error_code (error : error) : int =
  match error with
  | Failure -> 1
  | Error -> 2
  | CompileError -> 3
  | RuntimeError -> 4

let internal_err (err : Internal_error.t) : int =
  flush_all ();
  Log.elog "%a" Internal_error.pp err;
  Printexc.print_backtrace stderr;
  error_code Failure

let compile_err (err : Compile_error.t) : int =
  Log.elog ~header:false "%a" Compile_error.pp err;
  error_code CompileError

let runtime_err (err : Runtime_error.t) : int =
  Log.elog ~header:false "%a" Runtime_error.pp err;
  error_code RuntimeError

let eval_cmd (cmd : unit -> unit) : int =
  let open Ecma_sl in
  try cmd () |> fun () -> 0 with
  | Internal_error.Error err -> internal_err err
  | Compile_error.Error err -> compile_err err
  | Runtime_error.Error err -> runtime_err err
  | Command_error err -> error_code err
