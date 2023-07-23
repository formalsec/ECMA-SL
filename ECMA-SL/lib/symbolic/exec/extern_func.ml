type _ atype =
  | UArg : 'a atype -> (unit -> 'a) atype
  | Arg : 'a atype -> (Expr.t -> 'a) atype
  | Res : Expr.t atype

type _ func_type = Func : 'a atype -> 'a func_type
type extern_func = Extern_func : 'a func_type * 'a -> extern_func
