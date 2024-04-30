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

let list_iter ~(f : 'a -> unit Result.t) (lst : 'a list) : unit Result.t =
  let exception E of Result.cmderr in
  let list_f v = match f v with Error s -> raise (E s) | Ok () -> () in
  try List.iter list_f lst |> fun () -> Ok () with E s -> Error s

type observable =
  | Stdout of string
  | File of string

let observable_effects = [ Stdout "success"; Stdout "polluted"; File "success" ]

let find_effect (out : string) (effect : observable) : bool =
  match effect with
  | Stdout sub -> String.find_sub ~sub out |> Option.is_some
  | File file -> Sys.file_exists file

let execute_witness (env : Bos.OS.Env.t) (test : Fpath.t) (witness : Fpath.t) :
  observable option Result.t =
  Log.out "    running : %s@." @@ Fpath.to_string witness;
  let cmd = Bos.Cmd.(v "node" % p test % p witness) in
  let cmd_res = Bos.OS.Cmd.(run_out ~env ~err:err_run_out cmd |> out_string) in
  let* (out, status) = Result.bos cmd_res in
  match status with
  | (_, `Exited 0) -> Ok (List.find_opt (find_effect out) observable_effects)
  | (_, `Exited _) | (_, `Signaled _) -> Result.error (`SymNodeJS out)

let process_witness (env : string String.map) (input : Fpath.t)
  (witness : Fpath.t) : unit Result.t =
  let* effect = execute_witness env input witness in
  match effect with
  | Some (Stdout msg) ->
    Ok (Log.out "     status : true (\"%s\" in output)@." msg)
  | Some (File file) ->
    let* () = Result.bos (Bos.OS.Path.delete @@ Fpath.v file) in
    Ok (Log.out "     status : true (created file \"%s\")@." file)
  | None -> Ok (Log.out "     status : false (no side effect)@.")

let run () (opts : Options.t) : unit Result.t =
  Log.out "  replaying : %a...@." Fpath.pp opts.input;
  let env_map = [ ("NODE_PATH", Fmt.sprintf ".:%s" Share.nodejs) ] in
  let env = String.Map.of_list env_map in
  let witnesses_path = Fpath.(opts.testsuite / "witness-$(n).js") in
  let* witnesses = Result.bos (Bos.OS.Path.matches witnesses_path) in
  list_iter ~f:(process_witness env opts.input) witnesses
