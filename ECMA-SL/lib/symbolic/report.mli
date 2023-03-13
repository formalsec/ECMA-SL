type t
and testcase = (Z3.Sort.sort * Z3.Symbol.symbol * Z3.Expr.expr option) list
and testsuite = testcase list

val create : string -> int -> int -> int -> float -> float -> t
val add_testsuites : t -> testsuite -> testsuite -> t
val report_to_json : t -> string
val testsuite_to_json : t -> (string * string) list
