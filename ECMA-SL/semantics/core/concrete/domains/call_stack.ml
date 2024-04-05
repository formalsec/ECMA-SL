open EslBase
open EslSyntax.Source
module Stmt = EslSyntax.Stmt
module Func = EslSyntax.Func

type location =
  { lvl : int
  ; func : Func.t
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

let default () : 'store t = []

let create (func : Func.t) : 'store t =
  [ Toplevel { lvl = 0; func; stmt = Stmt.default () } ]

let loc (stack : 'store t) : location =
  match stack with
  | [] -> Internal_error.(throw __FUNCTION__ (Expecting "non-empty call stack"))
  | Toplevel loc :: _ | Intermediate (loc, _) :: _ -> loc

let level (stack : 'store t) : int = (loc stack).lvl
let func (stack : 'store t) : Func.t = (loc stack).func
let stmt (stack : 'store t) : Stmt.t = (loc stack).stmt

let pop (stack : 'store t) : 'store frame * 'store t =
  match stack with
  | [] -> Internal_error.(throw __FUNCTION__ (Expecting "non-empty call stack"))
  | frame :: stack' -> (frame, stack')

let push (stack : 'store t) (func : Func.t) (store : 'store)
  (cont : Stmt.t list) (retvar : string) : 'store t =
  let loc = { lvl = level stack + 1; func; stmt = Stmt.default () } in
  let restore = { store; cont; retvar } in
  Intermediate (loc, restore) :: stack

let update (stack : 'store t) (stmt : Stmt.t) : 'store t =
  let loc_f loc = { loc with stmt } in
  match stack with
  | [] -> Internal_error.(throw __FUNCTION__ (Expecting "non-empty call stack"))
  | Toplevel loc :: [] -> [ Toplevel { loc with stmt } ]
  | Intermediate (loc, r) :: frames -> Intermediate (loc_f loc, r) :: frames
  | _ -> Internal_error.(throw __FUNCTION__ (Unexpected "call stack format"))

let pp_loc (fmt : Fmt.t) (region : region) : unit =
  Fmt.fprintf fmt "file %S, line %d" region.file region.left.line

let pp_frame (fmt : Fmt.t) (frame : 'store frame) : unit =
  let frame_loc = function Toplevel loc | Intermediate (loc, _) -> loc in
  let { func; stmt; _ } = frame_loc frame in
  Fmt.fprintf fmt "'%s' in %a" (Func.name' func) pp_loc stmt.at

let pp (fmt : Fmt.t) (stack : 'store t) : unit =
  let open Fmt in
  let frame_loc = function Toplevel loc | Intermediate (loc, _) -> loc in
  let func_name frame = Func.name' (frame_loc frame).func in
  let pp_binding fmt frame = pp_print_string fmt (func_name frame) in
  if List.length stack = 0 then pp_str fmt "{}"
  else fprintf fmt "{ %a }" (pp_lst " <- " pp_binding) stack

let pp_tabular (fmt : Fmt.t) (stack : 'store t) : unit =
  let open Fmt in
  let pp_trace fmt frame = fprintf fmt "\nCalled from %a" pp_frame frame in
  match stack with
  | [] -> fprintf fmt "Empty call stack"
  | frame :: stack' ->
    fprintf fmt "%a%a" pp_frame frame (pp_lst "" pp_trace) stack'

let str ?(tabular : bool = false) (stack : 'store t) : string =
  Fmt.asprintf "%a" (if tabular then pp_tabular else pp) stack
