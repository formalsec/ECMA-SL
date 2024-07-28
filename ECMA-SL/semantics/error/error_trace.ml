open EslBase
open EslSyntax
open Call_stack

type store = Value.t Store.t
type t = store Call_stack.t

module RtTraceFmt (ErrorType : Error_type.ERROR_TYPE) = struct
  let pp_frame (ppf : Fmt.t) (frame : store frame) : unit =
    let { func; stmt; _ } = loc frame in
    Fmt.fmt ppf "@\nCalled from '%s' in %a" (Func.name' func) pp_loc stmt.at

  let pp (ppf : Fmt.t) (stack : t) : unit =
    let pp_stack = Fmt.(pp_lst !>"" pp_frame) in
    match stack with
    | [] -> Log.fail "expecting non-empty call stack"
    | frame :: stack' ->
      Fmt.fmt ppf "@\nRaised at %a%a" pp_frame frame pp_stack stack'

  let str (stack : t) : string = Fmt.str "%a" pp stack [@@inline]
end
