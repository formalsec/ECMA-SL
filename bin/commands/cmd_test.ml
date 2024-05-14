open Ecma_sl
open Syntax.Result

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
    }

  let set (inputs : Fpath.t list) (lang : Enums.Lang.t)
    (jsinterp : Enums.JSInterp.t) (harness : Fpath.t option)
    (test_type : Enums.JSTest.t) (report : Fpath.t option)
    (interp_profiler : Enums.InterpProfiler.t) : t =
    { inputs; lang; jsinterp; harness; test_type; report; interp_profiler }
end

module TestRecord = struct
  type result =
    | Success
    | Failure
    | Anomaly
    | Skipped

  type t =
    { input : Fpath.t
    ; output : Files.output
    ; name : string
    ; sections : string list
    ; test : string
    ; flags : string list
    ; error : Val.t option
    ; streams : Log.Redirect.t option
    ; retval : Val.t Result.t
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
    ; retval = Ok Null
    ; result = Skipped
    ; time = Base.time ()
    ; metrics = `Null
    }

  let pp_path (limit : int) (fmt : Fmt.t) (path : string) : unit =
    let (path', _) = String.truncate limit path in
    let len = String.length path' in
    let dots = if len < limit then String.make (limit - len) '.' else "" in
    Fmt.fprintf fmt "%s %s" path' dots

  let pp_error (fmt : Fmt.t) (error : Val.t option) : unit =
    match error with None -> Fmt.pp_str fmt "none" | Some v -> Val.pp fmt v

  let pp_retval (fmt : Fmt.t) (retval : Val.t Result.t) : unit =
    match retval with Ok v -> Val.pp fmt v | Error _ -> Fmt.pp_str fmt "-"

  let pp_result (fmt : Fmt.t) (result : result) : unit =
    match result with
    | Success -> Font.pp_text_out [ Green ] fmt "SUCCESS"
    | Failure -> Font.pp_text_out [ Red ] fmt "FAILURE"
    | Anomaly -> Font.pp_text_out [ Purple ] fmt "ANOMALY"
    | Skipped -> Font.pp_text_out [ Yellow ] fmt "SKIPPED"

  let pp_status (fmt : Fmt.t) (record : t) : unit =
    let open Fmt in
    let limit = !Options.term_width - 20 in
    let path = Fpath.to_string record.input in
    let (_, _, secs, millis) = Base.format_time record.time in
    let pp_time fmt (secs, millis) = fprintf fmt "[%02d.%03ds]" secs millis in
    let pp_cond_time = pp_cond (record.result != Skipped) pp_time in
    fprintf fmt "%a " (Font.pp_out [ Faint ] (pp_path limit)) path;
    fprintf fmt "%a " pp_result record.result;
    fprintf fmt "%a" (Font.pp_out [ Faint ] pp_cond_time) (secs, millis)

  let pp_report (fmt : Fmt.t) (record : t) : unit =
    let open Fmt in
    let line = String.make Options.report_width '-' in
    let pp_streams = pp_opt Log.Redirect.pp_captured in
    let pp_div fmt hdr = fprintf fmt "@\n%s@\n%s@\n%s@\n@\n" line hdr line in
    fprintf fmt "%s" record.test;
    fprintf fmt "%a%a" pp_div "Test Output:" pp_streams record.streams;
    fprintf fmt "%a" pp_div "Test Details:";
    fprintf fmt "name: %s@\n" record.name;
    fprintf fmt "sections: %a@\n" (pp_lst "/" pp_str) record.sections;
    fprintf fmt "flags: [%a]@\n" (pp_lst ", " pp_str) record.flags;
    fprintf fmt "error: %a@\n@\n" pp_error record.error;
    fprintf fmt "retval: %a@\n" pp_retval record.retval;
    fprintf fmt "result: %a@\n" pp_result record.result;
    if record.result != Skipped then fprintf fmt "time: %0.2fs@\n" record.time;
    if record.metrics != `Null then
      fprintf fmt "%a" Cmd_interpret.InterpreterMetrics.pp record.metrics
end

module TestTree = struct
  type t' =
    | Test of TestRecord.t
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

  let total (tree : t) : int =
    tree.success + tree.failure + tree.anomaly + tree.skipped

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
      Hashtbl.add tree.items record.name (Test record);
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

  let pp_status_header (fmt : Fmt.t) () : unit =
    let line = String.make (!Options.term_width - 1) '-' in
    let header = Fmt.sprintf "%s\n ECMA-SL Test Summary:\n" line in
    Font.pp_text_out [ Cyan ] fmt header

  let pp_summary_header (fmt : Fmt.t) () : unit =
    let line = String.make (!Options.term_width - 1) '-' in
    Fmt.fprintf fmt "%a@\n@\nTest Summary:@\n" (Font.pp_text_out [ Cyan ]) line

  let rec pp_status (fmt : Fmt.t) (tree : t) : unit =
    let pp_item fmt = function
      | Test record -> Fmt.fprintf fmt "%a@\n" TestRecord.pp_status record
      | Tree tree' -> pp_status fmt tree'
    in
    Fmt.pp_hashtbl "" (fun fmt (_, i) -> pp_item fmt i) fmt tree.items

  let rec pp_section (depth : int) (fmt : Fmt.t) (tree : t) : unit =
    let open Fmt in
    let pp_item fmt (_, i) =
      match i with Tree tree -> pp_section (depth + 1) fmt tree | _ -> ()
    in
    let pp_curr_section fmt tree =
      if depth > 0 then
        let indent = (depth - 1) * 2 in
        let limit = !Options.term_width - 32 - indent in
        let total = total tree in
        let ratio = float_of_int tree.success *. 100.0 /. float_of_int total in
        fprintf fmt "%s%a [%d / %d] (%.2f%%)@\n" (String.make indent ' ')
          (TestRecord.pp_path limit) tree.section tree.success total ratio
    in
    fprintf fmt "%a%a" pp_curr_section tree (pp_hashtbl "" pp_item) tree.items

  let pp_total (fmt : Fmt.t) (tree : t) : unit =
    let open Fmt in
    let total = total tree in
    let ratio = float_of_int tree.success *. 100.0 /. float_of_int total in
    let (_, mins, secs, millis) = Base.format_time tree.time in
    fprintf fmt "Tests Successful: %d / %d (%.2f%%) | " tree.success total ratio;
    fprintf fmt "Time elapsed: %dm %ds %dms@\n" mins secs millis;
    fprintf fmt "Failures: %d, Anomalies: %d, Skipped: %d" tree.failure
      tree.anomaly tree.skipped

  let pp_summary (fmt : Fmt.t) (tree : t) : unit =
    Fmt.fprintf fmt "@\n%a@\n%a@\n%a" pp_summary_header () (pp_section 0) tree
      pp_total tree

  let pp (fmt : Fmt.t) (tree : t) : unit =
    Fmt.fprintf fmt "%a@\n%a%a" pp_status_header () pp_status tree pp_summary
      tree
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

  let parse_test262_error (metadata : string) : Val.t option =
    let neg_f = Option.map (fun err -> Some (Val.Str err)) in
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
  let interp_config (profiler : Enums.InterpProfiler.t) :
    Cmd_interpret.Options.config =
    let interp_config = Cmd_interpret.Options.default_config () in
    let instrument = { interp_config.instrument with profiler } in
    { interp_config with instrument }

  let setup_environment (jsinterp : Enums.JSInterp.t) (harness : Fpath.t option)
    : (Prog.t * Val.t Heap.t option) Result.t =
    let interp_config = Cmd_interpret.Options.default_config () in
    Cmd_execute.setup_execution jsinterp harness interp_config

  let set_input_flags (record : TestRecord.t) : Fpath.t Result.t =
    let rec flag_appends = function
      | [] -> ""
      | "onlyStrict" :: flags' -> "\"use strict\";\n" ^ flag_appends flags'
      | _ :: flags' -> flag_appends flags'
    in
    if List.length record.flags == 0 then Ok record.input
    else
      let test = flag_appends record.flags ^ record.test in
      let input = Fpath.v (Filename.temp_file "ecmasl" "flagged-input.js") in
      let* () = Result.bos (Bos.OS.File.writef input "%s" test) in
      Ok input

  let unfold_result (result : Interpreter.result Result.t) :
    Val.t Result.t * Yojson.Basic.t =
    let retval_f res = res.Interpreter.retval in
    let metrics_f res = res.Interpreter.metrics in
    let retval = map retval_f result in
    let metrics = fold ~ok:metrics_f ~error:(fun _ -> `Null) result in
    (retval, metrics)

  let check_result (error : Val.t option) (retval : Val.t Result.t) :
    TestRecord.result =
    match (retval, error) with
    | (Ok (Tuple [ _; Symbol "normal"; _; _ ]), None) -> Success
    | (Ok (Tuple [ _; Symbol "throw"; e1; _ ]), Some e2) when Val.equal e1 e2 ->
      Success
    | (Ok (Tuple [ _; _; _; _ ]), _) -> Failure
    | (_, _) -> Anomaly

  let execute (env : Prog.t * Val.t Heap.t option) (record : TestRecord.t)
    (interp_profiler : Enums.InterpProfiler.t) : TestRecord.t Result.t =
    Log.debug "Starting test '%a'." Fpath.pp record.input;
    let interp_config = interp_config interp_profiler in
    let* input = set_input_flags record in
    let streams = Log.Redirect.capture Shared in
    let interp_result = Cmd_execute.execute_js env interp_config input in
    Log.Redirect.restore streams;
    let streams = Some streams in
    let (retval, metrics) = unfold_result interp_result in
    let result = check_result record.error retval in
    let time = Base.time () -. record.time in
    Ok { record with streams; retval; result; time; metrics }
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

let process_record (opts : Options.t) (env : Prog.t * Val.t Heap.t option)
  (workspace : Fpath.t) (input : Fpath.t) (output : Files.output) :
  TestRecord.t Result.t =
  let* record = record workspace input output in
  match Enums.Lang.resolve_file_lang ~warn:false [ JS ] input with
  | Some JS ->
    let* record' = TestParser.parse opts.test_type record in
    TestRunner.execute env record' opts.interp_profiler
  | _ -> Ok { record with result = Skipped }

let run_single (opts : Options.t) (env : Prog.t * Val.t Heap.t option)
  (tree : TestTree.t) (workspace : Fpath.t) (input : Fpath.t)
  (output : Files.output) : unit Result.t =
  let* record = process_record opts env workspace input output in
  let* record' = TestTree.add tree record record.sections in
  Fmt.printf "%a@." TestRecord.pp_status record';
  let* () = dump_record_report output record' in
  match record'.result with Success -> Ok () | _ -> Result.error `Test

let test_summary (output : Fpath.t option) (total_time : float)
  (tree : TestTree.t) : unit Result.t =
  TestTree.count_results tree;
  Log.out "%a@." TestTree.pp_summary { tree with time = total_time };
  match output with
  | Some dir when Fpath.is_dir_path dir -> dump_section_smry dir tree
  | Some path -> Result.bos (Bos.OS.File.writef path "%a@." TestTree.pp tree)
  | _ -> Ok ()

let run () (opts : Options.t) : unit Result.t =
  let total_time = Base.time () in
  let* inputs = Files.generate_input_list opts.inputs in
  Options.term_width := get_logging_width inputs;
  Log.out "%a@." TestTree.pp_status_header ();
  let* env = TestRunner.setup_environment opts.jsinterp opts.harness in
  let tree = TestTree.create "" in
  let outext = Enums.Lang.str TestReport in
  let run_single' = run_single opts env tree in
  let exitcode = Files.process_inputs ~outext run_single' inputs opts.report in
  let total_time = Base.time () -. total_time in
  let* () = test_summary opts.report total_time tree in
  exitcode
