type t
and testcase = (string * string * string) list
and testsuite = testcase list

val create :
  file:string ->
  paths:int ->
  errors:int ->
  unknowns:int ->
  analysis:float ->
  solver:float ->
  t

val add_testsuites : t -> final:testsuite -> error:testsuite -> t
val report_to_json : t -> string
val testsuite_to_json : t -> (string * string) list
