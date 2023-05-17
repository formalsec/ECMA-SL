open Core

type testcase = (string * string * string) list
type testsuite = testcase list

let serialise_report out_file file paths errors unknowns t_analysis t_solver =
  let data =
    "{ \"file\" : \"" ^ file ^ "\", \"paths\" : " ^ string_of_int paths
    ^ ", \"errors\" : " ^ string_of_int errors ^ ", \"unknowns\" : "
    ^ string_of_int unknowns ^ ", \"time_analysis\" : \""
    ^ string_of_float t_analysis ^ "\", \"time_solver\" : \""
    ^ string_of_float t_solver ^ "\" }"
  in
  Io.write_file ~file:out_file ~data

let test_to_json testcase : string list =
  List.map testcase ~f:(fun (sort, name, interp) ->
      sprintf "{ \"type\" : \"%s\", \"name\" : \"%s\", \"value\" : \"%s\" }"
        sort name interp)

let sink_to_json (sink, e) : string =
  sprintf "{ \"sink\" : \"%s\", \"expression\": \"%s\" }" sink e

let serialise_testsuite (testsuite : testsuite) (prefix : string) : unit =
  List.iteri testsuite ~f:(fun i t ->
      let data = sprintf "[ %s ]" (String.concat ~sep:", " (test_to_json t)) in
      Io.write_file ~file:(sprintf "%s-%d.json" prefix i) ~data)

let serialise_queries (queries : (string * string) list) (prefix : string) :
    unit =
  List.iteri queries ~f:(fun i (e, smt) ->
      Io.write_file ~file:(sprintf "%s-%d.pc" prefix i) ~data:e;
      Io.write_file ~file:(sprintf "%s-%d.smt2" prefix i) ~data:smt)

let serialise_sinks (sinks : (string * string) list) (prefix : string) : unit =
  List.iteri sinks ~f:(fun i sink ->
      let data = sink_to_json sink in
      Io.write_file ~file:(sprintf "%s-%d_sink.json" prefix i) ~data)
