type location =
  { func : Func.t
  ; mutable stmt : Stmt.t
  }

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

let no_stmt () : Stmt.t = Source.(Stmt.Skip @> no_region)
let create (func : Func.t) : 'store t = [ Toplevel { func; stmt = no_stmt () } ]

let loc (stack : 'store t) : Func.t * Stmt.t =
  match stack with
  | [] -> Eslerr.(internal __FUNCTION__ (Expecting "non-empty call stack"))
  | Toplevel loc :: _ | Intermediate (loc, _) :: _ -> (loc.func, loc.stmt)

let func (stack : 'store t) : Func.t = fst (loc stack)
let stmt (stack : 'store t) : Stmt.t = snd (loc stack)

let pop (stack : 'store t) : 'store frame * 'store t =
  match stack with
  | [] -> Eslerr.(internal __FUNCTION__ (Expecting "non-empty call stack"))
  | frame :: stack' -> (frame, stack')

let push (stack : 'store t) (func : Func.t) (store : 'store)
  (cont : Stmt.t list) (retvar : string) : 'store t =
  Intermediate ({ func; stmt = no_stmt () }, { store; cont; retvar }) :: stack

let update (stack : 'store t) (stmt : Stmt.t) : unit =
  match stack with
  | [] -> Eslerr.(internal __FUNCTION__ (Expecting "non-empty call stack"))
  | Toplevel loc :: _ | Intermediate (loc, _) :: _ -> loc.stmt <- stmt

let pp_loc (fmt : Fmt.t) (region : Source.region) : unit =
  let open Source in
  let { left; right } = region in
  Fmt.fprintf fmt "file %S, line %d, characters %d-%d" left.file left.line
    left.column right.column

let pp_frame (fmt : Fmt.t) (frame : 'store frame) : unit =
  let frame_loc = function Toplevel loc | Intermediate (loc, _) -> loc in
  let { func; stmt } = frame_loc frame in
  Fmt.fprintf fmt "'%s' in %a" func.it.name pp_loc stmt.at

let pp (fmt : Fmt.t) (stack : 'store t) : unit =
  let open Fmt in
  let frame_loc = function Toplevel loc | Intermediate (loc, _) -> loc in
  let func_name frame = (frame_loc frame).func.it.name in
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
  if tabular then Fmt.asprintf "%a" pp_tabular stack
  else Fmt.asprintf "%a" pp stack
