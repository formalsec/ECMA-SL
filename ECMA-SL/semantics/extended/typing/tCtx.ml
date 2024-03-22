open EslBase
open EslSyntax

type tenv = (Id.t', tvar) Hashtbl.t

and tvar =
  { tsig : EType.t option
  ; tref : EType.t
  }

let tvar_create (tsig : EType.t option) (tref : EType.t) : tvar = { tsig; tref }

type t =
  { prog : EProg.t
  ; func : EFunc.t
  ; tenv : tenv
  ; tsafe : bool
  }

let create (p : EProg.t) : t =
  { prog = p
  ; func = EFunc.default ()
  ; tenv = Hashtbl.create !Base.default_hashtbl_sz
  ; tsafe = true
  }

let prog (tctx : t) : EProg.t = tctx.prog
let func (tctx : t) : EFunc.t = tctx.func
let set_func (f : EFunc.t) (tctx : t) : t = { tctx with func = f }

let curr_treturn (tctx : t) : EType.t =
  EFunc.treturn tctx.func |> EType.resolve_topt

let safe_exec (f : t -> t) (tctx : t) : t =
  try f tctx
  with Typing_error.Error err ->
    Fmt.eprintf "%a@." Typing_error.pp err;
    { tctx with tsafe = false }

let tenv_reset (tctx : t) : unit = Hashtbl.reset tctx.tenv

let tenv_find (tctx : t) (x : Id.t) : tvar option =
  Hashtbl.find_opt tctx.tenv x.it

let tenv_set (tctx : t) (x : Id.t) (tvar : tvar) =
  Hashtbl.replace tctx.tenv x.it tvar
