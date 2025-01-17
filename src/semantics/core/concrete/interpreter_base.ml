open EslSyntax

type obj = Value.t Object.t
type store = Value.t Store.t
type heap = Value.t Heap.t
type stack = Value.t Store.t Call_stack.t

module IEntry = struct
  type t =
    { main : string
    ; heap : heap option
    }

  let default () : t = { main = "main"; heap = None }
end

module IResult = struct
  type t =
    { retval : Value.t
    ; heap : heap
    ; metrics : Yojson.Basic.t
    }

  let get_completion (heap : heap) loc =
    match Heap.get heap loc with
    | None -> Result.error (`Execute "Leaked invalid location")
    | Some obj -> (
      match Object.get obj "__completion__" with
      | None -> Result.error (`Execute "Object is not a completion")
      | Some _ ->
        let type_ = Object.get obj "type" |> Option.get in
        let value = Object.get obj "value" |> Option.get in
        let target = Object.get obj "target" |> Option.get in
        Ok (type_, value, target) )

  let is_normal_completion (heap : heap) (loc : int) =
    let open Smtml_prelude.Result in
    let+ (type_, _, _) = get_completion heap loc in
    match type_ with Str "normal" -> true | _ -> false
end

module IConst = struct
  let global_loc : Loc.t = 0
end

module IConfig = struct
  let print_depth : int option ref = ref None
  let resolve_exitval : bool ref = ref true
  let show_exitval : bool ref = ref false
end
