include EslBase
include EslSyntax

type obj = Smtml.Value.t Object.t
type store = Smtml.Value.t Store.t
type heap = Smtml.Value.t Heap.t
type stack = store Call_stack.t
type state = store * heap * stack
type cont = Stmt.t list
