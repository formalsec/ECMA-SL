include EslBase
include EslSyntax

type obj = Val.t Object.t
type store = Val.t Store.t
type heap = Val.t Heap.t
type stack = store Call_stack.t
type state = store * heap * stack
type cont = Stmt.t list
