open I2
open EslBase.Syntax.Result

type options =
  { filename : Fpath.t
  ; taint_summary : Fpath.t option
  ; workspace : Fpath.t
  }

let options filename taint_summary workspace =
  { filename; taint_summary; workspace }

let get_tests ~workspace (filename : Fpath.t) (taint_summary : Fpath.t option) =
  match taint_summary with
  | None -> Ok [ filename ]
  | Some conf ->
    let file = Fpath.to_string filename in
    let config = Fpath.to_string conf in
    let output = Fpath.(to_string @@ (workspace / "symbolic_test")) in
    Run.run ~file ~config ~output

let run_single ~(workspace : Fpath.t) (filename : Fpath.t) : int =
  let n = Cmd_symbolic.main { filename; entry_func = "main"; workspace } () in
  if n <> 0 then n else Cmd_replay.main { filename; workspace } ()

let run_all ({ filename; taint_summary; workspace } : options) =
  let* _ = Bos.OS.Dir.create workspace in
  let* symbolic_tests = get_tests ~workspace filename taint_summary in
  let rec loop = function
    | [] -> Ok 0
    | test :: remaning ->
      let workspace = Fpath.(workspace // rem_ext (base test)) in
      let n = run_single ~workspace test in
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
