open Bos
open Smtml.Syntax.Result

let symbolic = Tool.symbolic ()
let _ = [ symbolic ]
let dataset_dir = Fpath.v "./buckets.js"

let dataset =
  let+ dataset =
    OS.Dir.fold_contents
      (fun path acc -> if Fpath.has_ext ".js" path then path :: acc else acc)
      [] dataset_dir
  in
  (* Ensure deterministic execution order *)
  List.sort Fpath.compare dataset

let dup2 src dst =
  let src = Unix.openfile (Fpath.to_string src) [ O_CREAT; O_WRONLY ] 0o644 in
  Fun.protect ~finally:(fun () -> Unix.close src) (fun () -> Unix.dup2 src dst)

let run_single workspace file =
  let start_time = Unix.gettimeofday () in
  let start_rusage = Ruse.get Children in
  let pid = Unix.fork () in
  if pid = 0 then (
    dup2 Fpath.(workspace / "stderr") Unix.stderr;
    dup2 Fpath.(workspace / "stdout") Unix.stdout;
    let (prog, args) = Tool.cmd symbolic ~workspace ~file in
    Unix.execvp prog args )
  else
    let (waited_pid, status) = Unix.waitpid [] pid in
    let clock = Unix.gettimeofday () -. start_time in
    let rusage = Ruse.sub (Ruse.get Children) start_rusage in
    assert (waited_pid = pid);
    Exec_result.{ clock; file; rusage; status }

let result_dir =
  let tm = Unix.localtime @@ Unix.gettimeofday () in
  Fmt.kstr Fpath.v "./results-%04d%02d%02d_%02dh%02dm%02ds" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let results =
  let* dataset in
  (* Evaluated in reverse order *)
  list_map
    (fun file ->
      let workspace = Fpath.(result_dir // base file) in
      let+ _ = OS.Dir.create ~path:true workspace in
      let exec_result = run_single workspace file in
      Fmt.epr "%a@." Exec_result.pp exec_result;
      exec_result )
    dataset

let ok_or_failwith = function
  | Ok v -> v
  | Error (`Msg err) -> Fmt.failwith "%s@." err

let pp_summary fmt results =
  let (problems, clock_total) =
    List.fold_left
      (fun (prob_acc, clock_acc) Exec_result.{ clock; status; _ } ->
        let prob_acc =
          match status with Unix.WEXITED 0 -> prob_acc | _ -> succ prob_acc
        in
        (prob_acc, clock +. clock_acc) )
      (0, 0.0) results
  in
  let total = List.length results in
  if problems = 0 then Fmt.pr "All OK@\n"
  else Fmt.pf fmt "Tests %a/%a failed!@\n" Fmt.int problems Fmt.int total;
  Fmt.pf fmt "Elapsed time %as" Fmt.float clock_total

let () =
  let results = ok_or_failwith results in
  Fmt.pr "%a@." pp_summary results
