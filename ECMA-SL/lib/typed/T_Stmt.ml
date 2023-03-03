open Source
open E_Stmt

let type_stmt (stmt : E_Stmt.t) : unit =
  match stmt.it with
  | Skip -> failwith "[Type Checker] TODO: skip"
  | Fail _ -> failwith "[Type Checker] TODO: fail"
  | Throw _ -> failwith "[Type Checker] TODO: throw"
  | Print _ -> failwith "[Type Checker] TODO: print"
  | Assume _ -> failwith "[Type Checker] TODO: assume"
  | Assert _ -> failwith "[Type Checker] TODO: assert"
  | Return _ -> failwith "[Type Checker] TODO: return"
  | Wrapper (_, _) -> failwith "[Type Checker] TODO: wrapper"
  | Assign (_, _, _) -> failwith "[Type Checker] TODO: assign"
  | GlobAssign (_, _) -> failwith "[Type Checker] TODO: globassign"
  | Block _ -> failwith "[Type Checker] TODO: block"
  | If (_, _, _, _, _) -> failwith "[Type Checker] TODO: if"
  | EIf (_, _) -> failwith "[Type Checker] TODO: eif"
  | While (_, _) -> failwith "[Type Checker] TODO: while"
  | ForEach (_, _, _, _, _) -> failwith "[Type Checker] TODO: foreach"
  | FieldAssign (_, _, _) -> failwith "[Type Checker] TODO: fieldassign"
  | FieldDelete (_, _) -> failwith "[Type Checker] TODO: fielddelete"
  | ExprStmt _ -> failwith "[Type Checker] TODO: exprstmt"
  | RepeatUntil (_, _, _) -> failwith "[Type Checker] TODO: repeatuntil"
  | MatchWith (_, _) -> failwith "[Type Checker] TODO: matchwith"
  | MacroApply (_, _) -> failwith "[Type Checker] TODO: macroapply"
  | Switch (_, _, _, _) -> failwith "[Type Checker] TODO: switch"
  | Lambda (_, _, _, _, _) -> failwith "[Type Checker] TODO: lambda"
