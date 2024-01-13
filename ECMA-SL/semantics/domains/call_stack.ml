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

exception Empty_stack

let no_stmt () : Stmt.t = Source.(Stmt.Skip @> no_region)
let create (func : Func.t) : 'store t = [ Toplevel { func; stmt = no_stmt () } ]

let loc (stack : 'store t) : Func.t * Stmt.t =
  match stack with
  | [] -> raise Empty_stack
  | Toplevel loc :: _ | Intermediate (loc, _) :: _ -> (loc.func, loc.stmt)

let func (stack : 'store t) : Func.t = fst (loc stack)
let stmt (stack : 'store t) : Stmt.t = snd (loc stack)

let pop (stack : 'store t) : 'store frame * 'store t =
  match stack with [] -> raise Empty_stack | frame :: stack' -> (frame, stack')

let push (stack : 'store t) (func : Func.t) (store : 'store)
  (cont : Stmt.t list) (retvar : string) : 'store t =
  Intermediate ({ func; stmt = no_stmt () }, { store; cont; retvar }) :: stack

let update (stack : 'store t) (stmt : Stmt.t) : unit =
  match stack with
  | [] -> raise Empty_stack
  | Toplevel loc :: _ | Intermediate (loc, _) :: _ -> loc.stmt <- stmt

let pp_loc (fmt : Format.formatter) (region : Source.region) : unit =
  let open Source in
  let open Format in
  let { left; right } = region in
  fprintf fmt "file %S, line %d, characters %d-%d" left.file left.line
    left.column right.column

let pp_entry (fmt : Format.formatter) (frame : 'store frame) : unit =
  let open Format in
  let open Source in
  let frame_loc = function Toplevel loc | Intermediate (loc, _) -> loc in
  let { func; stmt } = frame_loc frame in
  fprintf fmt "'%s' in %a" func.it.name pp_loc stmt.at

let pp (fmt : Format.formatter) (stack : 'store t) : unit =
  let open Format in
  let frame_loc = function Toplevel loc | Intermediate (loc, _) -> loc in
  let func_name frame = (frame_loc frame).func.it.name in
  let pp_sep seq fmt () = pp_print_string fmt seq in
  let pp_lst seq pp fmt lst = pp_print_list ~pp_sep:(pp_sep seq) pp fmt lst in
  let pp_binding fmt frame = pp_print_string fmt (func_name frame) in
  fprintf fmt "{ %a }" (pp_lst " <- " pp_binding) stack

let pp_tabular (fmt : Format.formatter) (stack : 'store t) : unit =
  let open Format in
  let pp_sep seq fmt () = pp_print_string fmt seq in
  let pp_lst seq pp fmt lst = pp_print_list ~pp_sep:(pp_sep seq) pp fmt lst in
  let pp_curr fmt frame = fprintf fmt "Currently at %a" pp_entry frame in
  let pp_trace fmt frame = fprintf fmt "\nCalled from %a" pp_entry frame in
  match stack with
  | [] -> raise Empty_stack
  | frame :: stack' ->
    fprintf fmt "%a%a" pp_curr frame (pp_lst "" pp_trace) stack'

let str ?(tabular : bool = false) (stack : 'store t) : string =
  if tabular then Format.asprintf "%a" pp_tabular stack
  else Format.asprintf "%a" pp stack
