open Ecma_sl

type status =
  | Success
  | Failure
  | Error
  | CompileError
  | RuntimeError
  | EncodeError

exception Command_error of status

let status_code (status : status) : int =
  match status with
  | Success | Failure -> 1
  | Error -> 2
  | CompileError -> 3
  | RuntimeError -> 4
  | EncodeError -> 5

let internal_err (err : Internal_error.t) : int =
  flush_all ();
  Log.elog "%a" Internal_error.pp err;
  Printexc.print_backtrace stderr;
  status_code Failure

let compile_err (err : Compile_error.t) : int =
  Log.elog ~header:false "%a" Compile_error.pp err;
  status_code CompileError

let runtime_err (err : Runtime_error.t) : int =
  Log.elog ~header:false "%a" Runtime_error.pp err;
  status_code RuntimeError

let eval_cmd (cmd : unit -> unit) : int =
  try cmd () |> fun () -> 0 with
  | Internal_error.Error err -> internal_err err
  | Compile_error.Error err -> compile_err err
  | Runtime_error.Error err -> runtime_err err
  | Command_error status -> status_code status
