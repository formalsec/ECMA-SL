type stmt_eval =
  | SkipEval
  | MergeEval
  | DebugEval
  | BlockEval
  | PrintEval
  | ReturnEval
  | AssignEval
  | AssignCallEval of Func.t
  | AssignECallEval
  | AssignNewObjEval of string
  | AssignObjToListEval
  | AssignObjFieldsEval
  | AssignInObjCheckEval of string * string
  | FieldLookupEval of string * string
  | FieldAssignEval of string * string
  | FieldDeleteEval of string * string
  | IfEval of bool
  | WhileEval
  | FailEval
  | AssertEval of bool
  | AbortEval

module type M = sig
  type sl
  type state
  type return
  type 'a label
  type sl_label = sl label

  val parse_lvl : string -> sl
  val initial_state : unit -> state
  val generate_label : Stmt.t -> stmt_eval -> sl_label
  val eval_small_step : state -> sl_label -> return
  val next_state : return -> state
  val interceptor : string -> Val.t list -> Expr.t list -> sl_label option
end

module Default : M = struct
  type sl = unit
  type state = unit
  type return = unit
  type 'a label = unit
  type sl_label = unit

  let parse_lvl (_ : string) : sl = ()
  let initial_state () : state = ()
  let generate_label (_ : Stmt.t) (_ : stmt_eval) : sl_label = ()
  let eval_small_step (_ : state) (_ : sl_label) : return = ()
  let next_state (_ : return) : state = ()

  let interceptor (_ : string) (_ : Val.t list) (_ : Expr.t list) :
    sl_label option =
    None
end
