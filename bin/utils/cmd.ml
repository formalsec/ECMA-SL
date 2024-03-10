open EslCore
open Ecma_sl

type error =
  | Failure
  | Error
  | CompileError
  | RuntimeError

exception Command_error of error

let code_of_exn (exn : exn) : error =
  let open Ecma_sl in
  match exn with
  | Command_error err -> err
  | Eslerr.Internal_error _ -> Failure
  | Eslerr.Compile_error _ -> CompileError
  | Eslerr.Runtime_error _ -> RuntimeError
  | _ -> Failure

let error_code (error : error) : int =
  match error with
  | Failure -> 1
  | Error -> 2
  | CompileError -> 3
  | RuntimeError -> 4

let esl_internal_err (exn : exn) : int =
  flush_all ();
  Log.elog "%a" Eslerr.pp exn;
  Printexc.print_backtrace stderr;
  code_of_exn exn |> error_code

let esl_default_err (exn : exn) : int =
  Log.elog ~header:false "%a" Eslerr.pp exn;
  code_of_exn exn |> error_code

let eval_cmd (cmd : unit -> unit) : int =
  let open Ecma_sl in
  try cmd () |> fun () -> 0 with
  | Eslerr.Internal_error _ as exn -> esl_internal_err exn
  | Eslerr.Compile_error _ as exn -> esl_default_err exn
  | Eslerr.Runtime_error _ as exn -> esl_default_err exn
  | Command_error err -> error_code err
