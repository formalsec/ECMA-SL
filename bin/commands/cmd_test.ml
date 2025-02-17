open Ecma_sl
open Smtml_prelude.Result

module Options = struct
  let term_width : int ref = ref (Terminal.width Unix.stdout)
  let report_width = 80

  type t =
    { inputs : Fpath.t list
    ; lang : Enums.Lang.t
    ; jsinterp : Enums.JSInterp.t
    ; harness : Fpath.t option
    ; test_type : Enums.JSTest.t
    ; report : Fpath.t option
    ; interp_profiler : Enums.InterpProfiler.t
    ; webhook_url : string option
    ; jobs : int
    }

  let set (inputs : Fpath.t list) (lang : Enums.Lang.t)
    (jsinterp : Enums.JSInterp.t) (harness : Fpath.t option)
    (test_type : Enums.JSTest.t) (report : Fpath.t option)
    (interp_profiler : Enums.InterpProfiler.t) (webhook_url : string option)
    (jobs : int) : t =
    { inputs
    ; lang
    ; jsinterp
    ; harness
    ; test_type
    ; report
    ; interp_profiler
    ; webhook_url
    ; jobs
    }
end

module TestRecord = struct
  type result =
    | Success
    | Failure
    | Anomaly
    | Skipped

  type simple =
    { input : Fpath.t
    ; result : result
    ; time : float
    }

  type t =
    { input : Fpath.t
    ; output : Files.output
    ; name : string
    ; sections : string list
    ; test : string
    ; flags : string list
    ; error : Value.t option
    ; streams : Log.Redirect.t option
    ; retval : Value.t Result.t
    ; result : result
    ; time : float
    ; metrics : Yojson.Basic.t
    }

  let default () : t =
    { input = Fpath.v Filename.null
    ; output = `None
    ; name = ""
    ; sections = []
    ; test = ""
    ; flags = []
    ; error = None
    ; streams = None
    ; retval = Ok Nothing
    ; result = Skipped
    ; time = Base.time ()
    ; metrics = `Null
    }

  let simplify (record : t) : simple =
    let { input; result; time; _ } = record in
    { input; result; time }

  let pp_path (limit : int) (ppf : Format.formatter) (path : string) : unit =
    let (path', _) = String.truncate limit path in
    let len = String.length path' in
    let dots = if len < limit then String.make (limit - len) '.' else "" in
    Fmt.pf ppf "%s %s" path' dots

  let pp_error (ppf : Format.formatter) (error : Value.t option) : unit =
    match error with None -> Fmt.string ppf "none" | Some v -> Value.pp ppf v

  let pp_retval (ppf : Format.formatter) (retval : Value.t Result.t) : unit =
    match retval with Ok v -> Value.pp ppf v | Error _ -> Fmt.string ppf "-"

  let pp_result (ppf : Format.formatter) (result : result) : unit =
    match result with
    | Success -> Font.pp_text_out [ Green ] ppf "SUCCESS"
    | Failure -> Font.pp_text_out [ Red ] ppf "FAILURE"
    | Anomaly -> Font.pp_text_out [ Purple ] ppf "ANOMALY"
    | Skipped -> Font.pp_text_out [ Yellow ] ppf "SKIPPED"

  let pp_simple (ppf : Format.formatter) (record : simple) : unit =
    let limit = !Options.term_width - 20 in
    let path = Fpath.to_string record.input in
    let pp_time ppf time = Fmt.pf ppf "[%02.03f]" time in
    Fmt.pf ppf "%a " (Font.pp_out [ Faint ] (pp_path limit)) path;
    Fmt.pf ppf "%a " pp_result record.result;
    if record.result != Skipped then
      Fmt.pf ppf "%a" (Font.pp_out [ Faint ] pp_time) record.time

  let pp_report (ppf : Format.formatter) (record : t) : unit =
    let line = String.make Options.report_width '-' in
    let pp_streams = Fmt.option Log.Redirect.pp_captured in
    let pp_div ppf hdr = Fmt.pf ppf "@\n%s@\n%s@\n%s@\n@\n" line hdr line in
    let sep ppf () = Fmt.string ppf "/" in
    Fmt.pf ppf "%s" record.test;
    Fmt.pf ppf "%a%a" pp_div "Test Output:" pp_streams record.streams;
    Fmt.pf ppf "%a" pp_div "Test Details:";
    Fmt.pf ppf "name: %s@\n" record.name;
    Fmt.pf ppf "sections: %a@\n" Fmt.(list ~sep string) record.sections;
    Fmt.pf ppf "flags: [@[<h>%a@]]@\n" Fmt.(list ~sep:comma string) record.flags;
    Fmt.pf ppf "error: %a@\n@\n" pp_error record.error;
    Fmt.pf ppf "retval: %a@\n" pp_retval record.retval;
    Fmt.pf ppf "result: %a@\n" pp_result record.result;
    if record.result != Skipped then Fmt.pf ppf "time: %0.2fs@\n" record.time;
    if record.metrics != `Null then
      Fmt.pf ppf "%a" Cmd_interpret.InterpreterMetrics.pp record.metrics
end

module TestTree = struct
  type t' =
    | Test of TestRecord.simple
    | Tree of t

  and t =
    { section : string
    ; mutable time : float
    ; mutable success : int
    ; mutable failure : int
    ; mutable anomaly : int
    ; mutable skipped : int
    ; items : (string, t') Hashtbl.t
    }

  let create (section : string) : t =
    let (time, success, failure, anomaly, skipped) = (0.0, 0, 0, 0, 0) in
    let items = Hashtbl.create !Base.default_hashtbl_sz in
    { section; time; success; failure; anomaly; skipped; items }

  let total (tree : t) : int = tree.success + tree.failure + tree.anomaly

  let rename_test (record : TestRecord.t) : TestRecord.t =
    let rename_dup path =
      let (path', ext) = Fpath.split_ext path in
      Fpath.(v (to_string path' ^ "_1") + ext)
    in
    let rename_out = function
      | `Generated output' -> `Generated (rename_dup output')
      | output' -> output'
    in
    let input = rename_dup record.input in
    let output = rename_out record.output in
    let name = record.name ^ "_1" in
    { record with input; output; name }

  let rec add (tree : t) (record : TestRecord.t) (sections : string list) :
    TestRecord.t Result.t =
    match sections with
    | ([] | [ "." ]) when Hashtbl.mem tree.items record.name ->
      let record' = rename_test record in
      Log.warn "duplicated test identifier: renaming '%a' to '%a'" Fpath.pp
        record.input Fpath.pp record'.input;
      add tree record' sections
    | [] | [ "." ] ->
      Hashtbl.add tree.items record.name (Test (TestRecord.simplify record));
      Ok record
    | sec :: secs -> begin
      match Hashtbl.find_opt tree.items sec with
      | Some (Test _) -> Log.fail "unexpected test tree format"
      | Some (Tree tree') -> add tree' record secs
      | None ->
        let tree' = create sec in
        Hashtbl.add tree.items sec (Tree tree');
        add tree' record secs
    end

  let rec count_item (item : t') : float * int * int * int * int =
    match item with
    | Test { time; result = Success; _ } -> (time, 1, 0, 0, 0)
    | Test { time; result = Failure; _ } -> (time, 0, 1, 0, 0)
    | Test { time; result = Anomaly; _ } -> (time, 0, 0, 1, 0)
    | Test { result = Skipped; _ } -> (0.0, 0, 0, 0, 1)
    | Tree tree ->
      count_results tree;
      (tree.time, tree.success, tree.failure, tree.anomaly, tree.skipped)

  and count_results (tree : t) : unit =
    let count_f tree _ item =
      let (time, success, failure, anomaly, skipped) = count_item item in
      tree.time <- tree.time +. time;
      tree.success <- tree.success + success;
      tree.failure <- tree.failure + failure;
      tree.anomaly <- tree.anomaly + anomaly;
      tree.skipped <- tree.skipped + skipped
    in
    Hashtbl.iter (count_f tree) tree.items

  let pp_status_header (ppf : Format.formatter) () : unit =
    let line = String.make (!Options.term_width - 1) '-' in
    let header = Fmt.str "%s\n ECMA-SL Test Summary:\n" line in
    Font.pp_text_out [ Cyan ] ppf header

  let pp_summary_header (ppf : Format.formatter) () : unit =
    let line = String.make (!Options.term_width - 1) '-' in
    Fmt.pf ppf "%a@\n@\nTest Summary:@\n" (Font.pp_text_out [ Cyan ]) line

  let rec pp_status (ppf : Format.formatter) (tree : t) : unit =
    let pp_item ppf = function
      | Test record -> Fmt.pf ppf "%a@\n" TestRecord.pp_simple record
      | Tree tree' -> pp_status ppf tree'
    in
    Fmt.(hbox (hashtbl (fun ppf (_, i) -> pp_item ppf i)) ppf tree.items)

  let rec pp_section (depth : int) (ppf : Format.formatter) (tree : t) : unit =
    let open Fmt in
    let pp_item ppf (_, i) =
      match i with Tree tree -> pp_section (depth + 1) ppf tree | _ -> ()
    in
    let pp_curr_section ppf tree =
      if depth > 0 then
        let indent = (depth - 1) * 2 in
        let limit = !Options.term_width - 32 - indent in
        let total = total tree in
        let ratio = float_of_int tree.success *. 100.0 /. float_of_int total in
        pf ppf "%s%a [%d / %d] (%.2f%%)@\n" (String.make indent ' ')
          (TestRecord.pp_path limit) tree.section tree.success total ratio
    in
    pf ppf "%a%a" pp_curr_section tree (hbox (hashtbl pp_item)) tree.items

  let pp_total (ppf : Format.formatter) (tree : t) : unit =
    let open Fmt in
    let total = total tree in
    let ratio = float_of_int tree.success *. 100.0 /. float_of_int total in
    let (hours, mins, secs, millis) = Base.format_time tree.time in
    pf ppf "Tests Successful: %d / %d (%.2f%%) | " tree.success total ratio;
    if hours > 0 then
      pf ppf "Time elapsed: %dh %dm %ds %dms@\n" hours mins secs millis
    else pf ppf "Time elapsed: %dm %ds %dms@\n" mins secs millis;
    pf ppf "Failures: %d, Anomalies: %d, Skipped: %d" tree.failure tree.anomaly
      tree.skipped

  let pp_summary (ppf : Format.formatter) (tree : t) : unit =
    Fmt.pf ppf "@\n%a@\n%a@\n%a" pp_summary_header () (pp_section 0) tree
      pp_total tree

  let pp (ppf : Format.formatter) (tree : t) : unit =
    Fmt.pf ppf "%a@\n%a%a" pp_status_header () pp_status tree pp_summary tree
end

module TestParser = struct
  let regex (re : string) (text : string) : string option =
    try
      ignore (Str.search_forward (Str.regexp re) text 0);
      Some (Str.matched_group 1 text)
    with Not_found -> None

  let parse_test262_flags (metadata : string) : string list =
    let flags = regex "^flags: ?\\[\\(.+\\)\\]$" metadata in
    Option.fold ~none:[] ~some:(String.split_on_char ',') flags

  let parse_test262_error (metadata : string) : Value.t option =
    let neg_f = Option.map (fun err -> Some (Value.Str err)) in
    let negative_f md = regex "^negative: ?\\(.+\\)$" md |> neg_f in
    let errtype_f md = regex "^ +type: ?\\(.+\\)$" md |> neg_f in
    Option.value (negative_f metadata)
      ~default:(Option.value (errtype_f metadata) ~default:None)

  let parse_test262 (record : TestRecord.t) : TestRecord.t Result.t =
    match regex "^/\\*---\n\\(\\(.*\n\\)+\\)---\\*/" record.test with
    | None -> Error (`TestFmt "Invalid test format")
    | Some metadata ->
      let flags = parse_test262_flags metadata in
      let error = parse_test262_error metadata in
      Ok { record with flags; error }

  let parse_dispatcher (test_type : Enums.JSTest.t) (record : TestRecord.t) :
    TestRecord.t Result.t =
    match test_type with
    | Auto -> (
      match parse_test262 record with
      | Ok _ as record' -> record'
      | Error _ -> Ok record )
    | Simple -> Ok record
    | Test262 -> parse_test262 record

  let parse (test_type : Enums.JSTest.t) (record : TestRecord.t) :
    TestRecord.t Result.t =
    match parse_dispatcher test_type record with
    | Ok _ as record' -> record'
    | Error err -> Result.error err
end

module TestRunner = struct
  let test_skipped (record : TestRecord.t) : bool =
    let skipped_f skipped = function "skip" -> true | _ -> skipped in
    List.fold_left skipped_f false record.flags

  let interp_config (profiler : Enums.InterpProfiler.t) :
    Cmd_interpret.Options.config =
    let interp_config = Cmd_interpret.Options.default_config () in
    let instrument = { interp_config.instrument with profiler } in
    { interp_config with instrument }

  let set_test_flags (record : TestRecord.t) : Fpath.t Result.t =
    let flags_f (test, updated) = function
      | "onlyStrict" -> ("\"use strict\";\n" ^ test, true)
      | _ -> (test, updated)
    in
    let start = (record.test, false) in
    let (test, updated) = List.fold_left flags_f start record.flags in
    if not updated then Ok record.input
    else
      let input = Fpath.v (Filename.temp_file "ecmasl" "flagged-input.js") in
      let* () = Result.bos (Bos.OS.File.writef input "%s" test) in
      Ok input

  let unfold_result (result : Interpreter.IResult.t Result.t) :
    Value.t Result.t * Value.t Heap.t option * Yojson.Basic.t =
    match result with
    | Error _ as err -> (err, None, `Null)
    | Ok result -> (Ok result.retval, Some result.heap, result.metrics)

  let check_result (error : Value.t option) heap (retval : Value.t Result.t) :
    TestRecord.result =
    match (retval, heap) with
    | (Ok (App (`Op "loc", [ Int loc ])), Some heap) -> (
      match (Interpreter.IResult.get_completion heap loc, error) with
      | (Ok (App (`Op "symbol", [ Str "normal" ]), _, _), None) -> Success
      | (Ok (App (`Op "symbol", [ Str "throw" ]), e1, _), Some e2) ->
        if Value.equal e1 e2 then Success else Failure
      | (Ok (_, _, _), _) -> Failure
      | _ -> Anomaly )
    | _ -> Anomaly

  let execute (env : Prog.t * Value.t Heap.t option)
    (interp_config : Cmd_interpret.Options.config) (input : Fpath.t) :
    Interpreter.IResult.t Result.t =
    try Cmd_execute.execute_js env interp_config input
    with exn -> Result.error (`Generic (Printexc.to_string exn))

  let skip_test (record : TestRecord.t) : TestRecord.t Result.t =
    Ok { record with result = Skipped }

  let execute_test (env : Prog.t * Value.t Heap.t option)
    (record : TestRecord.t) (interp_profiler : Enums.InterpProfiler.t) :
    TestRecord.t Result.t =
    let interp_config = interp_config interp_profiler in
    let* input = set_test_flags record in
    let streams = Log.Redirect.capture Shared in
    let interp_result = execute env interp_config input in
    Log.Redirect.restore streams;
    let streams = Some streams in
    let (retval, heap, metrics) = unfold_result interp_result in
    let result = check_result record.error heap retval in
    let time = Base.time () -. record.time in
    Ok { record with streams; retval; result; time; metrics }

  let run (env : Prog.t * Value.t Heap.t option) (record : TestRecord.t)
    (interp_profiler : Enums.InterpProfiler.t) : TestRecord.t Result.t =
    Log.debug "Starting test '%a'." Fpath.pp record.input;
    if test_skipped record then skip_test record
    else execute_test env record interp_profiler
end

let get_logging_width (inputs : (Fpath.t * Fpath.t) list) : int =
  let path_len_f (_, p) = Fpath.to_string p |> String.length in
  let width = 32 + (List.map path_len_f inputs |> List.fold_left max 0) in
  min !Options.term_width width

let dump_record_report (output : Files.output) (record : TestRecord.t) :
  unit Result.t =
  let pp = TestRecord.pp_report in
  match (record.result, output) with
  | (Skipped, _) -> Ok ()
  | (_, `Generated path) -> Result.bos (Bos.OS.File.writef path "%a" pp record)
  | (_, _) -> Ok ()

let rec dump_section_smry (dir : Fpath.t) (tree : TestTree.t) : unit Result.t =
  let open Fpath in
  let dump_smry_f _ item acc =
    match ((item : TestTree.t'), acc) with
    | (_, (Error _ as err)) -> err
    | (Test _, Ok ()) -> Ok ()
    | (Tree tree', Ok ()) -> dump_section_smry (dir / tree'.section) tree'
  in
  let* () = Hashtbl.fold dump_smry_f tree.items (Ok ()) in
  let path = (dir / "report") + Enums.Lang.str TestSummary in
  Result.bos (Bos.OS.File.writef path "%a@." TestTree.pp tree)

let record (workspace : Fpath.t) (input : Fpath.t) (output : Files.output) :
  TestRecord.t Result.t =
  let rel = Option.get (Fpath.relativize ~root:workspace input) in
  let (id, name) = (Fpath.to_string rel, Fpath.filename rel) in
  let sections = String.split_on_char '/' (Filename.dirname id) in
  let* test = Result.bos (Bos.OS.File.read input) in
  Ok { (TestRecord.default ()) with input; output; name; sections; test }

let process_record (opts : Options.t) (env : Prog.t * Value.t Heap.t option)
  (workspace : Fpath.t) (input : Fpath.t) (output : Files.output) :
  TestRecord.t Result.t =
  let* record = record workspace input output in
  match Enums.Lang.resolve_file_lang ~warn:false [ JS ] input with
  | Some JS ->
    let* record' = TestParser.parse opts.test_type record in
    TestRunner.run env record' opts.interp_profiler
  | _ -> Ok { record with result = Skipped }

let mutex = Mutex.create ()

let run_single (opts : Options.t) (env : Prog.t * Value.t Heap.t option)
  (tree : TestTree.t) (is_parallel : bool) (workspace : Fpath.t)
  (input : Fpath.t) (output : Files.output) : unit Result.t =
  let* record = process_record opts env workspace input output in
  (* TODO: Clean this mutex up. Either TestTree is immutable and we fold and
     return a TestTreee from `process_inputs` or we make TestTree a thread-safe
     data structure *)
  Mutex.protect mutex @@ fun () ->
  let* record' = TestTree.add tree record record.sections in
  if is_parallel then
    Eio.traceln "%a" TestRecord.pp_simple (TestRecord.simplify record')
  else Log.stdout "%a@." TestRecord.pp_simple (TestRecord.simplify record');
  let* () = dump_record_report output record' in
  match record'.result with Success -> Ok () | _ -> Result.error `Test

let test_summary (is_parallel : bool) (output : Fpath.t option)
  (total_time : float) (tree : TestTree.t) : unit Result.t =
  TestTree.count_results tree;
  if is_parallel then
    Eio.traceln "%a" TestTree.pp_summary { tree with time = total_time }
  else Log.stdout "%a@." TestTree.pp_summary { tree with time = total_time };
  match output with
  | Some dir when Fpath.is_dir_path dir -> dump_section_smry dir tree
  | Some path -> Result.bos (Bos.OS.File.writef path "%a@." TestTree.pp tree)
  | _ -> Ok ()

(* Best effort to send a notification, not critical so just fail silently *)
let notify_done (tree : TestTree.t) (url : string) : unit =
  let url = Webhook.url_of_string url in
  let head = Git.get_head () in
  let title = Fmt.str "Test results (commit hash=%s) :octopus:" head in
  let body = Fmt.str "%a" TestTree.pp_total tree in
  let body = Webhook.default_slack_mrkdwn title body in
  Lwt_main.run @@ Webhook.post_and_forget url body

let run () ({ inputs; report; harness; jsinterp; jobs; _ } as opts : Options.t)
  : unit Result.t =
  let total_time = Base.time () in
  let* inputs = Files.generate_input_list inputs in
  Options.term_width := get_logging_width inputs;
  Log.stdout "%a@." TestTree.pp_status_header ();
  let* env = Cmd_execute.setup_execution true [] jsinterp harness in
  let tree = TestTree.create "" in
  let outext = Enums.Lang.str TestReport in
  let is_parallel = jobs > 1 in
  let run_single' = run_single opts env tree is_parallel in
  let exitcode = Files.process_inputs ~jobs ~outext run_single' inputs report in
  let total_time = Base.time () -. total_time in
  let* () = test_summary is_parallel report total_time tree in
  Option.iter (notify_done { tree with time = total_time }) opts.webhook_url;
  exitcode
