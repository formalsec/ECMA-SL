open Bos
open Ecma_sl
open Syntax.Result
module String = Astring.String

type options =
  { filename : Fpath.t
  ; testsuite : Fpath.t
  }

let options filename testsuite = { filename; testsuite }

let list_iter ~f lst =
  let exception E of Rresult.R.msg in
  try
    List.iter
      (fun v -> match f v with Error s -> raise (E s) | Ok () -> ())
      lst;
    Ok ()
  with E s -> Error s

let node test witness = Cmd.(v "node" % p test % p witness)

type observable =
  | Stdout of string
  | File of string

let observable_effects = [ Stdout "success"; Stdout "polluted"; File "success" ]

let env () =
  let node_path = Fmt.sprintf ".:%s" (List.hd Share.nodejs_location) in
  String.Map.of_list [ ("NODE_PATH", node_path) ]

let execute_witness ~env (test : Fpath.t) (witness : Fpath.t) =
  let open OS in
  Log.app "    running : %s@\n" @@ Fpath.to_string witness;
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
  | (_, `Exited _) | (_, `Signaled _) ->
    Error (`Msg (Fmt.sprintf "unexpected node failure: %s" out))

let replay filename testsuite =
  Log.app "  replaying : %a...@\n" Fpath.pp filename;
  let env = env () in
  let* witnesses = OS.Path.matches Fpath.(testsuite / "witness-$(n).js") in
  list_iter witnesses ~f:(fun witness ->
      let* effect = execute_witness ~env filename witness in
      match effect with
      | Some (Stdout msg) ->
        Log.app "     status : true (\"%s\" in output)@\n" msg;
        Ok ()
      | Some (File file) ->
        let* () = OS.Path.delete @@ Fpath.v file in
        Log.app "     status : true (created file \"%s\")@\n" file;
        Ok ()
      | None ->
        Log.app "     status : false (no side effect)@\n";
        Ok () )

let main (copts : Options.Common.t) opt =
  Options.Common.set copts;
  match replay opt.filename opt.testsuite with
  | Error (`Msg msg) ->
    Log.log ~header:false "%s" msg;
    1
  | Ok () -> 0
