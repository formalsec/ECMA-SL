open EslBase
open EslSyntax
open EslSyntax.Source

type location =
  { func : Func.t
  ; stmt : Stmt.t
  }

let location (location : location) : Func.t * Stmt.t =
  (location.func, location.stmt)

type 'store restore =
  { store : 'store
  ; cont : Stmt.t list
  ; retvar : string
  }

let restore (restore : 'store restore) : 'store * Stmt.t list * string =
  (restore.store, restore.cont, restore.retvar)

type 'store frame =
  | Toplevel of location
  | Intermediate of location * 'store restore

type 'store t = 'store frame list

let default () : 'store t = [] [@@inline]

let create (func : Func.t) : 'store t =
  [ Toplevel { func; stmt = Stmt.default () } ]
[@@inline]

let loc (frame : 'store frame) : location =
  match frame with Toplevel loc | Intermediate (loc, _) -> loc

let frame (stack : 'store t) : 'store frame =
  match stack with
  | [] -> Log.fail "expecting non-empty call stack"
  | frame :: _ -> frame

let depth (stack : 'store t) : int = List.length stack [@@inline]
let func (stack : 'store t) : Func.t = (loc @@ frame stack).func [@@inline]
let stmt (stack : 'store t) : Stmt.t = (loc @@ frame stack).stmt [@@inline]

let pop (stack : 'store t) : 'store frame * 'store t =
  match stack with
  | [] -> Log.fail "expecting non-empty call stack"
  | frame :: stack' -> (frame, stack')

let push (stack : 'store t) (func : Func.t) (store : 'store)
  (cont : Stmt.t list) (retvar : string) : 'store t =
  let loc = { func; stmt = Stmt.default () } in
  let restore = { store; cont; retvar } in
  Intermediate (loc, restore) :: stack

let update (stack : 'store t) (stmt : Stmt.t) : 'store t =
  match stack with
  | [] -> Log.fail "expecting non-empty call stack"
  | Toplevel loc :: stack' -> Toplevel { loc with stmt } :: stack'
  | Intermediate (loc, r) :: stack' ->
    Intermediate ({ loc with stmt }, r) :: stack'

let pp_loc (ppf : Fmt.t) (at : at) : unit =
  Fmt.fmt ppf "file %S, line %d" at.file at.lpos.line

let pp (ppf : Fmt.t) (stack : 'store t) : unit =
  let pp_bind ppf frame = Fmt.pp_str ppf (Func.name' (loc frame).func) in
  if depth stack == 0 then Fmt.pp_str ppf "{}"
  else Fmt.fmt ppf "{ %a }" Fmt.(pp_lst !>", " pp_bind) stack

let str (stack : 'store t) : string = Fmt.str "%a" pp stack [@@inline]
