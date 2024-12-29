open EslBase
open EslSyntax
open Call_stack

type store = Value.t Store.t
type t = store Call_stack.t

module RtTraceFmt (ErrorType : Error_type.ERROR_TYPE) = struct
  let pp_frame (ppf : Format.formatter) (frame : store frame) : unit =
    let { f; s; _ } = cursor frame in
    Fmt.pf ppf "@\nCalled from '%s' in %a" (Func.name' f) pp_loc s.at

  let pp (ppf : Format.formatter) (stack : t) : unit =
    let pp_stack = Fmt.(list pp_frame) in
    match stack with
    | [] -> Log.fail "expecting non-empty call stack"
    | frame :: stack' ->
      Fmt.pf ppf "@\nRaised at %a%a" pp_frame frame pp_stack stack'

  let str (stack : t) : string = Fmt.str "%a" pp stack [@@inline]
end
