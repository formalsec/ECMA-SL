type 'a err =
  [ `Abort of string
  | `Assert_failure of 'a
  | `Eval_failure of 'a
  | `Exec_failure of 'a
  | `ReadFile_failure of 'a
  | `Failure of string
  ]

module type Monad_type = sig
  type 'a t
end

module type T = sig
  type value
  type 'a choice
  type nonrec err = value err

  type _ atype =
    | UArg : 'a atype -> (unit -> 'a) atype
    | Arg : 'a atype -> (value -> 'a) atype
    | Res : (value, err) Result.t choice atype

  type _ func_type = Func : 'a atype -> 'a func_type
  type extern_func = Extern_func : 'a func_type * 'a -> extern_func
end

module Make (Value : Value_intf.T) (M : Monad_type) = struct
  type value = Value.value
  type 'a choice = 'a M.t
  type nonrec err = value err

  type _ atype =
    | UArg : 'a atype -> (unit -> 'a) atype
    | Arg : 'a atype -> (value -> 'a) atype
    | Res : (value, err) Result.t choice atype

  type _ func_type = Func : 'a atype -> 'a func_type
  type extern_func = Extern_func : 'a func_type * 'a -> extern_func
end
