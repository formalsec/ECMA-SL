open Source

module ErrMSg = struct
  let not_implemented (cons : string) : string =
    Printf.sprintf "The '%s' construct is not implemented." cons
end

type t = Success | TypeError of string

let format_type_error (stmt : E_Stmt.t) (msg : string) : string =
  let left = stmt.at.left in
  let stmt = E_Stmt.str stmt in
  let stmt' =
    if String.contains stmt ';' then
      String.sub stmt 0 (String.index stmt ';' + 1) ^ " ..."
    else stmt
  in
  Printf.sprintf "TypeError (%s:%d) : %s\n\t%s\n" left.file left.line msg stmt'
