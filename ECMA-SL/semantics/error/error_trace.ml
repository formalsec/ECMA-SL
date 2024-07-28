open EslBase
open EslSyntax

type t = Value.t Store.t Call_stack.t

module RtTraceFmt (ErrorType : Error_type.ERROR_TYPE) = struct
  let pp (ppf : Fmt.t) (trace : t) : unit =
    Fmt.fmt ppf "\nRaised at %a" Call_stack.pp_tabular trace

  let str (trace : t) : string = Fmt.str "%a" pp trace [@@inline]
end
