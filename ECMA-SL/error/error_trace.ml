open EslCore

type t = Val.t Store.t Call_stack.t

module RtTraceFmt (ErrorType : Error_type.ERROR_TYPE) = struct
  let pp (fmt : Fmt.t) (trace : t) : unit =
    Fmt.fprintf fmt "\nRaised at %a" Call_stack.pp_tabular trace

  let str (trace : t) : string = Fmt.asprintf "%a" pp trace
end
