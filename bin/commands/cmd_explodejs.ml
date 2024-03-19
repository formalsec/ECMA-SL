open I2
open EslBase.Syntax.Result

exception Out_of_time

type options =
  { filename : Fpath.t
  ; workspace : Fpath.t
  ; time_limit : float
  }

let options filename workspace = { filename; workspace; time_limit = 300. }

let get_tests ~workspace (filename : Fpath.t) =
  match Fpath.has_ext ".js" filename with
  | true -> Ok [ filename ]
  | false ->
    let config = Fpath.to_string filename in
    let output = Fpath.(to_string @@ (workspace / "symbolic_test")) in
    Run.run ~config ~output ()

let run_with_timeout limit f =
  let set_timer limit =
    ignore
    @@ Unix.setitimer Unix.ITIMER_REAL
         Unix.{ it_value = limit; it_interval = 0.01 }
  in
  let unset () =
    ignore
    @@ Unix.setitimer Unix.ITIMER_REAL Unix.{ it_value = 0.; it_interval = 0. }
  in
  set_timer limit;
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Out_of_time));
  let f () = try `Ok (f ()) with Out_of_time -> `Timeout in
  Fun.protect f ~finally:unset

let run_single ?(time_limit = 0.) ~(workspace : Fpath.t) (filename : Fpath.t) :
  int =
  let result =
    run_with_timeout time_limit (fun () ->
        let n =
          Cmd_symbolic.main { filename; entry_func = "main"; workspace } ()
        in
        if n <> 0 then n else Cmd_replay.main { filename; workspace } () )
  in
  match result with
  | `Ok n -> n
  | `Timeout ->
    Format.printf "Reached time_limit@.";
    1

let run_all ({ filename; workspace; time_limit } : options) =
  let* _ = Bos.OS.Dir.create workspace in
  let* symbolic_tests = get_tests ~workspace filename in
  let rec loop = function
    | [] -> Ok 0
    | test :: remaning ->
      let workspace = Fpath.(workspace // rem_ext (base test)) in
      let n = run_single ~time_limit ~workspace test in
      if n <> 0 then Error (`Status n) else loop remaning
  in
  loop symbolic_tests

let main opts () =
  match run_all opts with
  | Ok n -> n
  | Error (`Status n) ->
    Format.eprintf "error: Failed during symbolic execution/confirmation@.";
    n
  | Error (`Msg msg) ->
    Format.eprintf "error: %s@." msg;
    1
