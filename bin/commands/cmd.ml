type error =
  | Failure
  | Error

exception Command_error of error

let error_code (error : error) : int =
  match error with Failure -> 1 | Error -> 2

let log ?(esl_header : bool = true) (msg : string) : unit =
  if esl_header then Printf.eprintf "%s: %s\n" Sys.argv.(0) msg
  else Printf.eprintf "%s\n" msg

let eval_cmd (cmd : unit -> unit) : int =
  let open Ecma_sl in
  try cmd () |> fun () -> 0 with
  | Eslerr.Internal_error _ as exn ->
    flush_all ();
    log (Eslerr.format exn);
    Printexc.print_backtrace stderr;
    error_code Failure
  | Eslerr.Runtime_error _ as exn ->
    log (Eslerr.format exn);
    error_code Error
  | Command_error err -> error_code err

let test_file_ext (file : string) (exts : string list) : unit =
  if not (List.exists (Filename.check_suffix file) exts) then
    let ext_str = String.concat " or " exts in
    log (Printf.sprintf "expecting file with '%s' extension" ext_str)
