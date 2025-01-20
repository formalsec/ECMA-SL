open Ecma_sl

let regex (re : string) (text : string) : string option =
  try
    ignore (Str.search_forward (Str.regexp re) text 0);
    Some (Str.matched_group 1 text)
  with Not_found -> None

let parse_test262_flags (metadata : string) : string list =
  let flags = regex "^flags: ?\\[\\(.+\\)\\]$" metadata in
  Option.fold ~none:[] ~some:(String.split_on_char ',') flags

let parse_test262_error (metadata : string) : Value.t option =
  let neg_f = Option.map (fun err -> Value.Str err) in
  let negative_f md = regex "^negative: ?\\(.+\\)$" md |> neg_f in
  let errtype_f md = regex "^ +type: ?\\(.+\\)$" md |> neg_f in
  match negative_f metadata with Some _ as v -> v | None -> errtype_f metadata

let parse_test262 (record : Test_record.t) : Test_record.t Result.t =
  match regex "^/\\*---\n\\(\\(.*\n\\)+\\)---\\*/" record.test with
  | None -> Error (`TestFmt "Invalid test format")
  | Some metadata ->
    let flags = parse_test262_flags metadata in
    let error = parse_test262_error metadata in
    Ok { record with flags; error }

let parse (test_type : Enums.JSTest.t) (record : Test_record.t) :
  Test_record.t Result.t =
  match test_type with
  | Simple -> Ok record
  | Test262 -> parse_test262 record
  | Auto -> (
    match parse_test262 record with
    | Ok _ as record' -> record'
    | Error _ -> Ok record )
