open EslBase
open EslSyntax

type t = Val.t Store.t Call_stack.t

module RtTraceFmt (ErrorType : Error_type.ERROR_TYPE) = struct
  let pp (ppf : Fmt.t) (trace : t) : unit =
    Fmt.format ppf "\nRaised at %a" Call_stack.pp_tabular trace

  let str (trace : t) : string = Fmt.str "%a" pp trace
end
