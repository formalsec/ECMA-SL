type msg =
  | Default
  | Custom of string
  | Expecting of string
  | UnexpectedEval of string option
  | NotImplemented of string option

module InternalErr : Error_type.ERROR_TYPE with type t = msg = struct
  type t = msg

  let header : string = "InternalError"
  let font : Font.t = [ Red ]

  let equal (msg1 : t) (msg2 : t) : bool =
    match (msg1, msg2) with
    | (Default, Default) -> true
    | (Custom msg1', Custom msg2') -> String.equal msg1' msg2'
    | (Expecting msg1', Expecting msg2') -> String.equal msg1' msg2'
    | (UnexpectedEval msg1', UnexpectedEval msg2') ->
      Option.equal String.equal msg1' msg2'
    | (NotImplemented msg1', NotImplemented msg2') ->
      Option.equal String.equal msg1' msg2'
    | _ -> false

  let pp (fmt : Fmt.t) (msg : t) : unit =
    let open Fmt in
    match msg with
    | Default -> fprintf fmt "generic internal error"
    | Custom msg' -> fprintf fmt "%s" msg'
    | Expecting msg' -> fprintf fmt "expecting %s" msg'
    | UnexpectedEval None -> fprintf fmt "unexpected evaluation"
    | UnexpectedEval (Some msg') ->
      fprintf fmt "unexpected evaluation of '%s'" msg'
    | NotImplemented None -> fprintf fmt "not implemented"
    | NotImplemented (Some msg') -> fprintf fmt "'%s' not implemented" msg'

  let str (msg : t) : string = Fmt.asprintf "%a" pp msg
end

type t =
  { msg : msg
  ; loc : string
  }

exception Error of t

let raise (err : t) : 'a = Stdlib.raise (Error err)
let create (loc : string) (msg : msg) : t = { msg; loc }
let throw (loc : string) (msg : msg) : 'a = raise @@ create loc msg

let pp (fmt : Fmt.t) (err : t) : unit =
  Fmt.fprintf fmt "(%s) %a" err.loc InternalErr.pp err.msg

let str (err : t) = Fmt.asprintf "%a" pp err
