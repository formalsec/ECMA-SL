open Bos
open Ecma_sl
open Ecma_sl.Syntax.Result
module String = Astring.String

type options =
  { filename : Fpath.t
  ; workspace : Fpath.t
  }

let options filename workspace = { filename; workspace }
let node test witness = Cmd.(v "node" % p test % p witness)

type observable =
  | Stdout of string
  | File of string

let observable_effects = [ Stdout "success"; Stdout "polluted"; File "success" ]

let env testsuite =
  let ws = Unix.realpath @@ Fpath.to_string testsuite in
  let sharejs = List.hd Share.Location.nodejs in
  let node_path = Fmt.asprintf ".:%s:%s" ws sharejs in
  String.Map.of_list [ ("NODE_PATH", node_path) ]

let execute_witness ~env (test : Fpath.t) (witness : Fpath.t) =
  let open OS in
  Log.app "    running : %a" Fpath.pp witness;
  let cmd = node test witness in
  let+ (out, status) = Cmd.(run_out ~env ~err:err_run_out cmd |> out_string) in
  ( match status with
  | (_, `Exited 0) -> ()
  | (_, `Exited _) | (_, `Signaled _) ->
    Fmt.printf "unexpected node failure: %s" out );
  List.find_opt
    (fun effect ->
      match effect with
      | Stdout sub -> String.find_sub ~sub out |> Option.is_some
      | File file -> Sys.file_exists file )
    observable_effects

let replay filename workspace =
  Log.app "  replaying : %a..." Fpath.pp filename;
  let* testsuite = OS.Dir.must_exist Fpath.(workspace / "test-suite") in
  let env = env testsuite in
  let* witnesses = OS.Path.matches Fpath.(testsuite / "witness-$(n).js") in
  list_iter witnesses ~f:(fun witness ->
      let* effect = execute_witness ~env filename witness in
      match effect with
      | Some (Stdout msg) ->
        Log.app "     status : true (\"%s\" in output)" msg;
        Ok ()
      | Some (File file) ->
        let* () = OS.Path.delete @@ Fpath.v file in
        Log.app "     status : true (created file \"%s\")" file;
        Ok ()
      | None ->
        Log.app "     status : false (no side effect)";
        Ok () )

let main { filename; workspace } () =
  match replay filename workspace with
  | Error (`Msg msg) ->
    Log.log ~header:false "%s" msg;
    1
  | Ok () -> 0
