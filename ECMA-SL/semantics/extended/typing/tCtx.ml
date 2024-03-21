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
  ; tenv : tenv
  ; tsafe : bool
  }

let create (p : EProg.t) : t =
  { prog = p; tenv = Hashtbl.create !Base.default_hashtbl_sz; tsafe = true }

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
