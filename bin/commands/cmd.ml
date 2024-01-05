type error =
  | Failure
  | Error

exception Command_error of error

let error_code (error : error) : int =
  match error with Failure -> 1 | Error -> 2

let log ?(header : bool = true) msg_fmt =
  let header_str = if header then "ecma-sl: " else "" in
  Format.(kdprintf (eprintf "%s%t" header_str) msg_fmt)

let eval_cmd (cmd : unit -> unit) : int =
  let open Ecma_sl in
  try cmd () |> fun () -> 0 with
  | Eslerr.Internal_error _ as exn ->
    flush_all ();
    log "%s" (Eslerr.format exn);
    Printexc.print_backtrace stderr;
    error_code Failure
  | Eslerr.Runtime_error _ as exn ->
    log ~header:false "%s" (Eslerr.format exn);
    error_code Error
  | Command_error err -> error_code err

let test_file_ext (file : string) (exts : string list) : unit =
  if not (List.exists (Filename.check_suffix file) exts) then
    let ext_str = String.concat " or " exts in
    log "expecting file with '%s' extension" ext_str
