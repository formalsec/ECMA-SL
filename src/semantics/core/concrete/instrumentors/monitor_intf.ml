open EslSyntax

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
  | AssignNewObjEval of Loc.t
  | AssignObjToListEval
  | AssignObjFieldsEval
  | AssignInObjCheckEval of Loc.t * string
  | FieldLookupEval of Loc.t * string
  | FieldAssignEval of Loc.t * string
  | FieldDeleteEval of Loc.t * string
  | IfEval of bool
  | WhileEval
  | SwitchEval of Value.t
  | FailEval
  | AssertEval of bool
  | AbortEval

let pp_stmt_eval fmt = function
  | SkipEval -> Fmt.pf fmt "SkipEval"
  | MergeEval -> Fmt.pf fmt "MergeEval"
  | DebugEval -> Fmt.pf fmt "DebugEval"
  | BlockEval -> Fmt.pf fmt "BlockEval"
  | PrintEval -> Fmt.pf fmt "PrintEval"
  | ReturnEval -> Fmt.pf fmt "ReturnEval"
  | AssignEval -> Fmt.pf fmt "AssignEval"
  | AssignCallEval _ -> Fmt.pf fmt "AssignCallEval "
  | AssignECallEval -> Fmt.pf fmt "AssignECallEval"
  | AssignNewObjEval _ -> Fmt.pf fmt "AssignNewObjEval "
  | AssignObjToListEval -> Fmt.pf fmt "AssignObjToListEval"
  | AssignObjFieldsEval -> Fmt.pf fmt "AssignObjFieldsEval"
  | AssignInObjCheckEval _ -> Fmt.pf fmt "AssignInObjCheckEval "
  | FieldLookupEval _ -> Fmt.pf fmt "FieldLookupEval "
  | FieldAssignEval _ -> Fmt.pf fmt "FieldAssignEval "
  | FieldDeleteEval _ -> Fmt.pf fmt "FieldDeleteEval "
  | IfEval _ -> Fmt.pf fmt "IfEval "
  | WhileEval -> Fmt.pf fmt "WhileEval"
  | SwitchEval _ -> Fmt.pf fmt "SwitchEval "
  | FailEval -> Fmt.pf fmt "FailEval"
  | AssertEval _ -> Fmt.pf fmt "AssertEval "
  | AbortEval -> Fmt.pf fmt "AbortEval"

module type S = sig
  type sl
  type state
  type return
  type 'a label

  type t =
    { mutable state : state
    ; mutable label : sl label
    }

  val initial_state : unit -> t
  val set_label : t -> sl label -> unit
  val update_label : t -> Stmt.t -> stmt_eval -> unit
  val eval_small_step : t -> unit
  val interceptor : string -> Value.t list -> Expr.t list -> sl label option
end
