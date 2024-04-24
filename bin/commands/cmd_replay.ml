open Bos
open Ecma_sl
open Ecma_sl.Syntax.Result
module String = Astring.String

module Options = struct
  type t =
    { input : Fpath.t
    ; testsuite : Fpath.t
    }

  let set (input : Fpath.t) (testsuite : Fpath.t) : t = { input; testsuite }
end

let list_iter ~f lst =
  let exception E of Rresult.R.msg in
  let list_f v = match f v with Error s -> raise (E s) | Ok () -> () in
  try List.iter list_f lst |> fun () -> Ok () with E s -> Error s

type observable =
  | Stdout of string
  | File of string

let observable_effects = [ Stdout "success"; Stdout "polluted"; File "success" ]

let env () =
  let node_path = Fmt.sprintf ".:%s" Share.nodejs in
  String.Map.of_list [ ("NODE_PATH", node_path) ]

let node test witness = Cmd.(v "node" % p test % p witness)

let execute_witness (env : OS.Env.t) (test : Fpath.t) (witness : Fpath.t) =
  let open OS in
  Log.out "    running : %s@." @@ Fpath.to_string witness;
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

let replay (input : Fpath.t) (testsuite : Fpath.t) =
  Log.out "  replaying : %a...@." Fpath.pp input;
  let env = env () in
  let* witnesses = OS.Path.matches Fpath.(testsuite / "witness-$(n).js") in
  list_iter witnesses ~f:(fun witness ->
      let* effect = execute_witness env input witness in
      match effect with
      | Some (Stdout msg) ->
        Log.out "     status : true (\"%s\" in output)@." msg;
        Ok ()
      | Some (File file) ->
        let* () = OS.Path.delete @@ Fpath.v file in
        Log.out "     status : true (created file \"%s\")@." file;
        Ok ()
      | None ->
        Log.out "     status : false (no side effect)@.";
        Ok () )

let run () (opts : Options.t) : unit =
  match replay opts.input opts.testsuite with
  | Ok () -> ()
  | Error (`Msg msg) ->
    Log.out "%s@." msg;
    raise (Exec.Command_error Failure)
