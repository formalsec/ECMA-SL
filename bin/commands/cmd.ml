type error =
  | Failure
  | Error
  | CompileError
  | RuntimeError

exception Command_error of error

let code_of_exn (exn : exn) : error =
  let open Ecma_sl in
  match exn with
  | Eslerr.Internal_error _ -> Failure
  | Eslerr.Compile_error _ -> CompileError
  | Eslerr.Runtime_error _ -> RuntimeError
  | Command_error err -> err
  | _ -> Failure

let error_code (error : error) : int =
  match error with
  | Failure -> 1
  | Error -> 2
  | CompileError -> 3
  | RuntimeError -> 4

let log ?(header : bool = true) msg_fmt =
  let header_str = if header then "ecma-sl: " else "" in
  Format.(kdprintf (eprintf "%s%t@." header_str) msg_fmt)

let test_file_ext (langs : Lang.t list) (file : string) : Lang.t =
  match Lang.test_file_ext langs (Filename.extension file) with
  | Some lang -> lang
  | None ->
    let open Format in
    let pp_sep seq fmt () = pp_print_string fmt seq in
    let pp_lst seq pp fmt lst = pp_print_list ~pp_sep:(pp_sep seq) pp fmt lst in
    log "expecting file extensions: { %a }" (pp_lst " ; " Lang.pp) langs;
    raise (Command_error Error)

let eval_cmd (cmd : unit -> unit) : int =
  let open Ecma_sl in
  try cmd () |> fun () -> 0 with
  | Eslerr.Internal_error _ as exn ->
    flush_all ();
    log "%a" Eslerr.pp exn;
    Printexc.print_backtrace stderr;
    code_of_exn exn |> error_code
  | (Eslerr.Compile_error _ | Eslerr.Runtime_error _) as exn ->
    log ~header:false "%a" Eslerr.pp exn;
    code_of_exn exn |> error_code
  | Command_error err -> error_code err
