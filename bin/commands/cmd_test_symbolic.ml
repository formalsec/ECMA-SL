open Bos
open Ecma_sl
open Smtml_prelude.Result

module Symbolic_interpreter =
  Ecma_sl_symbolic.Symbolic_interpreter.Make (Ecma_sl_symbolic.Symbolic_error)

module Test = struct
  type t =
    { path : Fpath.t
    ; mutable metadata : Test_metadata.t option
    ; mutable result : Test_result.t option
    ; mutable time : float
    }

  let make ?metadata path = { path; metadata; result = None; time = 0.0 }

  let pp fmt { path; result; time; _ } =
    Fmt.pf fmt "%a ... %a [%02.03fs]" Fpath.pp path
      (Fmt.option Test_result.pp)
      result time

  let enumerate paths =
    OS.Path.fold ~elements:`Files ~traverse:`Any
      (fun path acc -> if Fpath.has_ext ".js" path then path :: acc else acc)
      [] paths
end

let parse paths =
  let+ inputs = Test.enumerate paths in
  (* Turn parsing of tests lazy *)
  let inputs = Cont.of_list inputs in
  Cont.map
    (fun path ->
      let metadata =
        match Test_metadata.Parser.parse @@ Fpath.to_string path with
        | Error _ -> None
        | Ok metadata -> Some metadata
      in
      Test.make ?metadata path )
    inputs

module Value = Ecma_sl_symbolic.Symbolic.Value
module Memory = Ecma_sl_symbolic.Symbolic_memory
module Object = Ecma_sl_symbolic.Symbolic_object
module Thread = Ecma_sl_symbolic.Choice_monad.Thread

let get_completion mem loc =
  match Memory.get mem loc with
  | None -> Error "Leaked invalid location"
  | Some obj -> (
    match Object.get obj (Value.string "__completion__") with
    | [] -> Error "Object is not a completion"
    | _ ->
      let type_ = Object.get obj (Value.string "type") |> List.hd |> fst in
      let value = Object.get obj (Value.string "value") |> List.hd |> fst in
      let target = Object.get obj (Value.string "target") |> List.hd |> fst in
      Ok (Smtml.Expr.view type_, Smtml.Expr.view value, Smtml.Expr.view target)
    )

let check_result (test : Test.t) thread result =
  let error = Option.bind test.metadata Test_metadata.error in
  match result with
  | Error _ -> test.result <- Some Anomaly
  | Ok result -> (
    match Smtml.Expr.view result with
    | List
        [ Hc.{ node = Val False; _ }
        ; Hc.{ node = Val (App (`Op "loc", [ Int loc ])); _ }
        ] -> begin
      let mem = Thread.mem thread in
      match (get_completion mem loc, error) with
      | (Ok (Val (App (`Op "symbol", [ Str "normal" ])), _, _), None) ->
        test.result <- Some Success
      | (Ok (Val (App (`Op "symbol", [ Str "throw" ])), Val a, _), Some b) ->
        if Smtml.Value.equal a b then test.result <- Some Success
        else test.result <- Some Failure
      | (Ok (_, _, _), _) -> test.result <- Some Failure
      | _ -> test.result <- Some Anomaly
    end
    | List [ Hc.{ node = Val True; _ }; _ ] -> test.result <- Some Anomaly
    | _ -> test.result <- Some Anomaly )

let run_and_check_result prelude results ({ Test.path; metadata; _ } as test) =
  let strict =
    match metadata with
    | None -> false
    | Some m -> List.mem "onlyStrict" m.flags
  in
  (* TODO: strict mode *)
  let filename =
    match Cmd_symbolic.setup_prelude ~strict path prelude with
    | Error (`Msg err) -> failwith err
    | Ok filename -> filename
  in
  match Cmd_symbolic.prog_of_js filename with
  | Error _err -> test.result <- Some Anomaly
  | Ok prog ->
    let start = Sys.time () in
    let _ =
      Symbolic_interpreter.run ~no_stop_at_failure:false
        ~out_cb:(check_result test)
        ~err_cb:(fun _ err -> [ err ])
        path prog
    in
    test.time <- Sys.time () -. start;
    Fmt.pr "%a@." Test.pp test;
    Queue.push test results

let print_and_count_results results =
  let succ = ref 0
  and fail = ref 0
  and anomaly = ref 0
  and skip = ref 0
  and total = ref 0
  and time = ref 0.0 in
  while not @@ Queue.is_empty results do
    let test = Queue.pop results in
    let () =
      match test.Test.result with
      | Some Success -> incr succ
      | Some Failure -> incr fail
      | Some Anomaly -> incr anomaly
      | Some Skipped -> incr skip
      | None -> ()
    in
    incr total;
    time := !time +. test.time
  done;
  let results_str =
    Fmt.str
      "@\n\
       Tests Successful: %d / %d (%02.02f%%) | Time elapsed: %02.03f@\n\
       Failures: %d, Anomalies: %d, Skipped: %d"
      !succ !total
      (float !succ /. float !total *. 100.0)
      !time !fail !anomaly !skip
  in
  Fmt.pr "%s@." results_str

(** This command is similar to `test` but it's for symbolic execution *)
let run ~prelude ~inputs =
  Result.bos
  @@
  let+ tests = parse inputs in
  let results = Queue.create () in
  let () = tests (run_and_check_result prelude results) in
  print_and_count_results results
