type t =
  [ `Abort of string
  | `Assert_failure of Smtml.Expr.t
  | `Eval_failure of Smtml.Expr.t
  | `Exec_failure of Smtml.Expr.t
  | `ReadFile_failure of Smtml.Expr.t
  | `Failure of string
  ]

let pp fmt = function
  | `Abort msg -> Fmt.pf fmt "      abort : %s@." msg
  | `Assert_failure v ->
    Fmt.pf fmt "     assert : failure with (%a)" Smtml.Expr.pp v
  | `Eval_failure v -> Fmt.pf fmt "       eval : %a" Smtml.Expr.pp v
  | `Exec_failure v -> Fmt.pf fmt "       exec : %a" Smtml.Expr.pp v
  | `ReadFile_failure v -> Fmt.pf fmt "   readFile : %a" Smtml.Expr.pp v
  | `Failure msg -> Fmt.pf fmt "    failure : %s" msg

let to_json = function
  | `Abort msg -> `Assoc [ ("type", `String "Abort"); ("sink", `String msg) ]
  | `Assert_failure v ->
    let v = Smtml.Expr.to_string v in
    `Assoc [ ("type", `String "Assert failure"); ("sink", `String v) ]
  | `Eval_failure v ->
    let v = Smtml.Expr.to_string v in
    `Assoc [ ("type", `String "Eval failure"); ("sink", `String v) ]
  | `Exec_failure v ->
    let v = Smtml.Expr.to_string v in
    `Assoc [ ("type", `String "Exec failure"); ("sink", `String v) ]
  | `ReadFile_failure v ->
    let v = Smtml.Expr.to_string v in
    `Assoc [ ("type", `String "ReadFile failure"); ("sink", `String v) ]
  | `Failure msg ->
    `Assoc [ ("type", `String "Failure"); ("sink", `String msg) ]

