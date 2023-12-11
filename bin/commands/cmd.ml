type error =
  | Failure
  | Error
  | Unsupported

exception CmdError of error

let err_code (error : error) : int =
  match error with Failure -> 1 | Error -> 2 | Unsupported -> 3

let log (msg : string) : unit = Printf.eprintf "ecma-sl: %s.\n" msg

let eval_cmd (cmd : unit -> unit) : int =
  try cmd () |> fun () -> 0 with CmdError err -> err_code err

let test_file_ext (file : string) (exts : string list) : unit =
  if not (List.exists (Filename.check_suffix file) exts) then
    let ext_str = String.concat " or " exts in
    log (Printf.sprintf "expecting file with '%s' extension" ext_str)
