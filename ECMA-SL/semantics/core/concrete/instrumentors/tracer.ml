open EslSyntax

type obj = Val.t Object.t
type heap = Val.t Heap.t
type heapval = heap * Val.t

module type M = sig
  val trace_at : bool ref
  val trace_expr : Expr.t -> heapval -> unit
  val trace_stmt : Stmt.t -> unit
  val trace_call : int -> Expr.t -> Expr.t list -> unit
  val trace_return : Func.t option -> heapval -> unit
end

module Disable : M = struct
  let trace_at = ref false
  let trace_expr (_ : Expr.t) (_ : heapval) : unit = ()
  let trace_stmt (_ : Stmt.t) : unit = ()
  let trace_call (_ : int) (_ : Expr.t) (_ : Expr.t list) : unit = ()
  let trace_return (_ : Func.t option) (_ : heapval) : unit = ()
end

module Call : M = struct
  let trace_at = ref false
  let trace_expr (_ : Expr.t) (_ : heapval) : unit = ()
  let trace_stmt (_ : Stmt.t) : unit = ()
  let trace_call (_ : int) (_ : Expr.t) (_ : Expr.t list) : unit = ()
  let trace_return (_ : Func.t option) (_ : heapval) : unit = ()
end

module Step : M = struct
  let trace_at = ref false
  let trace_expr (_ : Expr.t) (_ : heapval) : unit = ()
  let trace_stmt (_ : Stmt.t) : unit = ()
  let trace_call (_ : int) (_ : Expr.t) (_ : Expr.t list) : unit = ()
  let trace_return (_ : Func.t option) (_ : heapval) : unit = ()
end

module Full : M = struct
  let trace_at = ref false
  let trace_expr (_ : Expr.t) (_ : heapval) : unit = ()
  let trace_stmt (_ : Stmt.t) : unit = ()
  let trace_call (_ : int) (_ : Expr.t) (_ : Expr.t list) : unit = ()
  let trace_return (_ : Func.t option) (_ : heapval) : unit = ()
end

module Core : M = struct
  let trace_at = ref false
  let trace_expr (_ : Expr.t) (_ : heapval) : unit = ()
  let trace_stmt (_ : Stmt.t) : unit = ()
  let trace_call (_ : int) (_ : Expr.t) (_ : Expr.t list) : unit = ()
  let trace_return (_ : Func.t option) (_ : heapval) : unit = ()
end
