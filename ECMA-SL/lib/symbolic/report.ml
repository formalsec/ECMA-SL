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

and testcase = (Z3.Sort.sort * Z3.Symbol.symbol * Z3.Expr.expr option) list
and testsuite = testcase list

let create file paths errors unknowns analysis solver : t =
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

let add_testsuites (report : t) (final : testsuite) (error : testsuite) : t =
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
    List.map
      (fun (sort, name, interp) ->
        let sort' = Z3.Sort.to_string sort
        and name' = Z3.Symbol.to_string name
        and interp' = Option.map_default Z3.Expr.to_string "" interp in
        "{ \"type\" : \"" ^ sort' ^ "\", \"name\" : \"" ^ name'
        ^ "\", \"value\" : \"" ^ interp' ^ "\" }")
      testcase
  in
  List.mapi
    (fun i test ->
      let test_str = "[ " ^ String.concat ", " (testcase_to_json test) ^ " ]" in
      ("testcase-" ^ string_of_int i ^ ".json", test_str))
    report.final_testsuite
  @ List.mapi
      (fun i test ->
        let test_str =
          "[ " ^ String.concat ", " (testcase_to_json test) ^ " ]"
        in
        ("witness-" ^ string_of_int i ^ ".json", test_str))
      report.error_testsuite
