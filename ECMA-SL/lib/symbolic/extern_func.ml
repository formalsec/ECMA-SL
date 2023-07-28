module type T = sig
  type value

  type _ atype =
    | UArg : 'a atype -> (unit -> 'a) atype
    | Arg : 'a atype -> (value -> 'a) atype
    | Res : value atype

  type _ func_type = Func : 'a atype -> 'a func_type
  type extern_func = Extern_func : 'a func_type * 'a -> extern_func
end

module Make (Value : Value_intf.T) = struct
  type value = Value.value

  type _ atype =
    | UArg : 'a atype -> (unit -> 'a) atype
    | Arg : 'a atype -> (value -> 'a) atype
    | Res : value atype

  type _ func_type = Func : 'a atype -> 'a func_type
  type extern_func = Extern_func : 'a func_type * 'a -> extern_func
end
