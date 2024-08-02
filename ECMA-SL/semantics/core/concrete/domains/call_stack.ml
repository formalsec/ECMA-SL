open EslBase
open EslSyntax
open EslSyntax.Source

type cursor =
  { f : Func.t
  ; s : Stmt.t
  }

type 'store restore =
  { store : 'store
  ; cont : Stmt.t list
  ; retvar : string
  }

let restore (restore : 'store restore) : 'store * Stmt.t list * string =
  (restore.store, restore.cont, restore.retvar)

type 'store frame =
  | Toplevel of cursor
  | Intermediate of cursor * 'store restore

type 'store t = 'store frame list

let default () : 'store t = [] [@@inline]

let create (f : Func.t) : 'store t = [ Toplevel { f; s = Stmt.default () } ]
[@@inline]

let cursor (frame : 'store frame) : cursor =
  match frame with Toplevel cursor | Intermediate (cursor, _) -> cursor

let frame (stack : 'store t) : 'store frame =
  match stack with
  | [] -> Log.fail "expecting non-empty call stack"
  | frame :: _ -> frame

let depth (stack : 'store t) : int = List.length stack [@@inline]
let func (stack : 'store t) : Func.t = (cursor @@ frame stack).f [@@inline]
let stmt (stack : 'store t) : Stmt.t = (cursor @@ frame stack).s [@@inline]

let pop (stack : 'store t) : 'store frame * 'store t =
  match stack with
  | [] -> Log.fail "expecting non-empty call stack"
  | frame :: stack' -> (frame, stack')

let push (stack : 'store t) (f : Func.t) (store : 'store) (cont : Stmt.t list)
  (retvar : string) : 'store t =
  let cursor = { f; s = Stmt.default () } in
  let restore = { store; cont; retvar } in
  Intermediate (cursor, restore) :: stack

let update (stack : 'store t) (s : Stmt.t) : 'store t =
  match stack with
  | [] -> Log.fail "expecting non-empty call stack"
  | Toplevel cursor :: stack' -> Toplevel { cursor with s } :: stack'
  | Intermediate (cursor, r) :: stack' ->
    Intermediate ({ cursor with s }, r) :: stack'

let pp_loc (ppf : Fmt.t) (at : at) : unit =
  Fmt.fmt ppf "file %S, line %d" at.file at.lpos.line

let pp (ppf : Fmt.t) (stack : 'store t) : unit =
  let pp_bind ppf frame = Fmt.pp_str ppf (Func.name' (cursor frame).f) in
  if depth stack == 0 then Fmt.pp_str ppf "{}"
  else Fmt.fmt ppf "{ %a }" Fmt.(pp_lst !>", " pp_bind) stack

let str (stack : 'store t) : string = Fmt.str "%a" pp stack [@@inline]
