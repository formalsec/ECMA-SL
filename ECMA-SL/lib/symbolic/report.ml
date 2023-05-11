open Core

type t = {
  file : string;
  paths : int;
  errors : int;
  unknowns : int;
  analysis : float;
  solver : float;
  final_testsuite : testsuite;
  error_testsuite : testsuite;
}

and testcase = (string * string * string) list
and testsuite = testcase list

let create ~file ~paths ~errors ~unknowns ~analysis ~solver : t =
  {
    file;
    paths;
    errors;
    unknowns;
    analysis;
    solver;
    final_testsuite = [];
    error_testsuite = [];
  }

let add_testsuites (report : t) ~(final : testsuite) ~(error : testsuite) : t =
  { report with final_testsuite = final; error_testsuite = error }

let report_to_json (report : t) : string =
  "{ \"file\" : \"" ^ report.file ^ "\", \"paths\" : "
  ^ string_of_int report.paths ^ ", \"errors\" : "
  ^ string_of_int report.errors
  ^ ", \"unknowns\" : "
  ^ string_of_int report.unknowns
  ^ ", \"time_analysis\" : \""
  ^ string_of_float report.analysis
  ^ "\", \"time_solver\" : \""
  ^ string_of_float report.solver
  ^ "\" }"

(**
 * @Returns (filename * testcase) * list
 *)
let testsuite_to_json (report : t) : (string * string) list =
  let testcase_to_json (testcase : testcase) =
    List.map testcase ~f:(fun (sort, name, interp) ->
        "{ \"type\" : \"" ^ sort ^ "\", \"name\" : \"" ^ name
        ^ "\", \"value\" : \"" ^ interp ^ "\" }")
  in
  List.mapi report.final_testsuite ~f:(fun i test ->
      let test_str =
        "[ " ^ String.concat ~sep:", " (testcase_to_json test) ^ " ]"
      in
      ("testcase-" ^ string_of_int i ^ ".json", test_str))
  @ List.mapi report.error_testsuite ~f:(fun i test ->
        let test_str =
          "[ " ^ String.concat ~sep:", " (testcase_to_json test) ^ " ]"
        in
        ("witness-" ^ string_of_int i ^ ".json", test_str))
