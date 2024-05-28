open EslBase
open EslSyntax
open EslBase.Fmt

module Config = struct
  let trace_loc : bool ref = ref false
  let trace_depth : int option ref = ref None
  let max_obj_depth : int = 4
  let width : int = Terminal.width Unix.stderr
end

module Truncate = struct
  let prepare (str_el : 'a -> string) (el : 'a) : string * int =
    str_el el |> fun s -> (s, String.length s)

  let limit_indent (lvl : int) : int = Config.width - (lvl * 2)

  let limit_el (lim : int) (factor : int) (weight : int) (rest : int) : int =
    max (lim * weight / factor) (lim - rest)

  let pp (lim : int) (pp_el : t -> 'a -> unit) (ppf : Fmt.t) (el : 'a) : unit =
    let extra = Font.str_text_err [ Font.Faint ] "..." in
    let text = asprintf "%a" pp_el el in
    let (text', trunc) = String.truncate (lim - 3) text in
    pp_str ppf (if trunc then text' ^ extra else text')
end

type obj = Val.t Object.t
type heap = Val.t Heap.t
type heapval = heap * Val.t

let indent_pp (ppf : Fmt.t) (lvl : int) : unit =
  let indent = Array.make lvl "| " |> Array.to_list |> String.concat "" in
  Font.pp_text_err [ Faint ] ppf indent

let region_pp (limit : int) (ppf : Fmt.t) (at : Source.region) : unit =
  let open Source in
  let pp_region' ppf at = fprintf ppf "(%s:%d)" at.file at.left.line in
  Font.pp_err [ Italic; Faint ] (Truncate.pp limit pp_region') ppf at

let cond_region_pp (lvl : int) (ppf : Fmt.t) (at : Source.region) : unit =
  let limit = Truncate.limit_indent lvl in
  let pp ppf () = fprintf ppf "\n%a%a" indent_pp lvl (region_pp limit) at in
  pp_cond !Config.trace_loc pp ppf ()

let rec heapval_pp ?(depth : int = 0) (heap : heap) (ppf : Fmt.t) (v : Val.t) :
  unit =
  match v with
  | Loc l when depth < Config.max_obj_depth -> (
    match Heap.get heap l with
    | Ok obj -> Object.pp (heapval_pp ~depth:(depth + 1) heap) ppf obj
    | _ -> pp_str ppf "{ ??? }" )
  | _ -> Val.pp ppf v

let val_pp (limit : int) (ppf : Fmt.t) (v_str : string) : unit =
  (Font.pp_err [ Cyan ] (Truncate.pp limit pp_str)) ppf v_str

let heapval (heap : heap) (v : Val.t) : string =
  asprintf "%a" (heapval_pp heap) v

module CallFmt = struct
  let pp_func_restore (ppf : Fmt.t) (f : Func.t) : unit =
    let limit = Truncate.limit_indent 0 - 8 in
    fprintf ppf "%a started%a"
      (Font.pp_err [ Cyan ] (Truncate.pp limit pp_str))
      (Func.name' f) (cond_region_pp 1) f.at

  let pp_func_call (ppf : Fmt.t) ((lvl, s) : int * Stmt.t) : unit =
    let limit = Truncate.limit_indent lvl - 7 in
    let pp_stmt = Font.pp_err [ Cyan ] (Truncate.pp limit Source.Code.pp) in
    fprintf ppf "%a%a called%a" indent_pp lvl pp_stmt s.at
      (cond_region_pp (lvl + 1))
      s.at

  let retval_format (v : Val.t) : string * Val.t =
    match v with
    | Tuple [ Bool false; v' ] -> ("returned ", v')
    | Tuple [ Bool true; err ] -> ("throwed ", err)
    | _ -> ("returned ", v)

  let pp_func_return (heap : heap) (ppf : Fmt.t)
    ((lvl, f, s, v) : int * Func.t * Stmt.t * Val.t) : unit =
    let (retval_header, v') = retval_format v in
    let (fn_str, fn_len) = Truncate.prepare Func.name' f in
    let (v_str, v_len) = Truncate.prepare (heapval heap) v' in
    let limit = Truncate.limit_indent lvl - String.length retval_header - 1 in
    let limit_f = Truncate.limit_el limit 4 3 v_len in
    let limit_v = Truncate.limit_el limit 4 1 fn_len in
    fprintf ppf "%a%a %s%a%a" indent_pp lvl
      (Font.pp_err [ Cyan ] (Truncate.pp limit_f pp_str))
      fn_str retval_header (val_pp limit_v) v_str (cond_region_pp lvl) s.at
end

module type CODE_FMT = sig
  val log_expr : Expr.t -> bool
  val log_stmt : Stmt.t -> bool
  val expr_str : Expr.t -> string
  val stmt_pp : Fmt.t -> Stmt.t -> unit
end

module DefaultFmt (CodeFmt : CODE_FMT) = struct
  module CodeFmt = CodeFmt

  let pp_expr (heap : heap) (ppf : Fmt.t) ((lvl, e, v) : int * Expr.t * Val.t) :
    unit =
    let lvl' = lvl + 1 in
    let (e_str, e_len) = Truncate.prepare CodeFmt.expr_str e in
    let (v_str, v_len) = Truncate.prepare (heapval heap) v in
    let limit = Truncate.limit_indent lvl' - 11 in
    let limit_e = Truncate.limit_el limit 2 1 v_len in
    let limit_v = Truncate.limit_el limit 2 1 e_len in
    let pp_eval = Font.pp_text_err [ Italic ] in
    let pp_expr = Truncate.pp limit_e pp_str in
    fprintf ppf "%a- %a %a -> %a" indent_pp lvl' pp_eval "eval" pp_expr e_str
      (val_pp limit_v) v_str

  let pp_stmt (ppf : Fmt.t) ((lvl, s) : int * Stmt.t) : unit =
    let lvl' = lvl + 1 in
    let limit = Truncate.limit_indent lvl' in
    let pp_stmt = Font.pp_err [ Cyan ] (Truncate.pp limit CodeFmt.stmt_pp) in
    fprintf ppf "%a%a%a" indent_pp lvl' pp_stmt s (cond_region_pp lvl') s.at

  let pp_func (header : string) (ppf : Fmt.t) ((lvl, f) : int * Func.t) : unit =
    let limit = Truncate.limit_indent lvl - String.length header - 1 in
    let pp_fname = Font.pp_err [ Cyan ] (Truncate.pp limit pp_str) in
    fprintf ppf "%a%s %a" indent_pp lvl header pp_fname (Func.name' f)
end

let log_level (lvl : int) : bool =
  let log_lvl' max_lvl = lvl < max_lvl in
  Option.fold ~none:true ~some:log_lvl' !Config.trace_depth

module EslCodeFmt : CODE_FMT = struct
  let log_expr (e : Expr.t) : bool =
    e.at.real && match e.it with Val _ -> false | _ -> true

  let log_stmt (s : Stmt.t) : bool =
    s.at.real && match s.it with Skip | Merge | Block _ -> false | _ -> true

  let expr_str (e : Expr.t) : string = Source.Code.str e.at
  let stmt_pp (ppf : Fmt.t) (s : Stmt.t) : unit = Source.Code.pp ppf s.at
end

module CeslCodeFmt : CODE_FMT = struct
  let log_expr (e : Expr.t) : bool =
    match e.it with Val _ -> false | _ -> true

  let log_stmt (s : Stmt.t) : bool =
    match s.it with Skip | Merge | Block _ -> false | _ -> true

  let expr_str (e : Expr.t) : string = Expr.str e
  let stmt_pp (ppf : Fmt.t) (s : Stmt.t) : unit = Stmt.pp ppf s
end

module type M = sig
  val trace_expr : int -> Expr.t -> heapval -> unit
  val trace_stmt : int -> Stmt.t -> unit
  val trace_restore : int -> Func.t -> unit
  val trace_call : int -> Func.t -> Stmt.t -> unit
  val trace_return : int -> Func.t -> Stmt.t -> heapval -> unit
end

module Disable : M = struct
  let trace_expr (_ : int) (_ : Expr.t) (_ : heapval) : unit = ()
  let trace_stmt (_ : int) (_ : Stmt.t) : unit = ()
  let trace_restore (_ : int) (_ : Func.t) : unit = ()
  let trace_call (_ : int) (_ : Func.t) (_ : Stmt.t) : unit = ()
  let trace_return (_ : int) (_ : Func.t) (_ : Stmt.t) (_ : heapval) : unit = ()
end

module Call : M = struct
  open CallFmt

  let trace_expr (_ : int) (_ : Expr.t) (_ : heapval) : unit = ()
  let trace_stmt (_ : int) (_ : Stmt.t) : unit = ()

  let trace_restore (lvl : int) (f : Func.t) : unit =
    if lvl == -1 then Log.err "%a@." pp_func_restore f

  let trace_call (lvl : int) (_ : Func.t) (s : Stmt.t) : unit =
    if log_level lvl then Log.err "%a@." pp_func_call (lvl, s)

  let trace_return (lvl : int) (f : Func.t) (s : Stmt.t) ((heap, v) : heapval) :
    unit =
    if log_level lvl then Log.err "%a@." (pp_func_return heap) (lvl, f, s, v)
end

module Step : M = struct
  open DefaultFmt (EslCodeFmt)
  open CodeFmt

  let trace_expr (_ : int) (_ : Expr.t) (_ : heapval) : unit = ()

  let trace_stmt (lvl : int) (s : Stmt.t) : unit =
    if log_level lvl && log_stmt s then Log.err "%a@." pp_stmt (lvl, s)

  let trace_restore (lvl : int) (f : Func.t) : unit =
    match lvl with
    | -1 -> Log.err "%a@." (pp_func "starting on function") (0, f)
    | _ ->
      if log_level lvl then
        Log.err "%a@." (pp_func "returning to function") (lvl, f)

  let trace_call (lvl : int) (f : Func.t) (_ : Stmt.t) : unit =
    if log_level lvl then Log.err "%a@." (pp_func "entering function") (lvl, f)

  let trace_return (lvl : int) (f : Func.t) (_ : Stmt.t) (_ : heapval) : unit =
    if log_level lvl then Log.err "%a@." (pp_func "exiting function") (lvl, f)
end

module Full : M = struct
  open DefaultFmt (EslCodeFmt)
  open CodeFmt

  let trace_expr (lvl : int) (e : Expr.t) ((heap, v) : heapval) : unit =
    if log_level lvl && log_expr e then Log.err "%a@." (pp_expr heap) (lvl, e, v)

  let trace_stmt (lvl : int) (s : Stmt.t) : unit = Step.trace_stmt lvl s
  let trace_restore (lvl : int) (f : Func.t) : unit = Step.trace_restore lvl f

  let trace_call (lvl : int) (f : Func.t) (s : Stmt.t) : unit =
    Step.trace_call lvl f s

  let trace_return (lvl : int) (f : Func.t) (s : Stmt.t) (hv : heapval) : unit =
    Step.trace_return lvl f s hv
end

module Core : M = struct
  open DefaultFmt (CeslCodeFmt)
  open CodeFmt

  let trace_expr (lvl : int) (e : Expr.t) ((heap, v) : heapval) : unit =
    if log_level lvl && log_expr e then Log.err "%a@." (pp_expr heap) (lvl, e, v)

  let trace_stmt (lvl : int) (s : Stmt.t) : unit =
    if log_level lvl && log_stmt s then Log.err "%a@." pp_stmt (lvl, s)

  let trace_restore (lvl : int) (f : Func.t) : unit =
    match lvl with
    | -1 -> Log.err "%a@." (pp_func "starting on function") (0, f)
    | _ ->
      if log_level lvl then
        Log.err "%a@." (pp_func "returning to function") (lvl, f)

  let trace_call (lvl : int) (f : Func.t) (_ : Stmt.t) : unit =
    if log_level lvl then Log.err "%a@." (pp_func "entering function") (lvl, f)

  let trace_return (lvl : int) (f : Func.t) (_ : Stmt.t) (_ : heapval) : unit =
    if log_level lvl then Log.err "%a@." (pp_func "exiting function") (lvl, f)
end
