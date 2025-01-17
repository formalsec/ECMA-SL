module Type = struct
  type t =
    | In of in_channel
    | Out of out_channel

  let get_in = function
    | In ic -> Ok ic
    | Out _ -> Error (`Failure "cannot use 'out_channel' for input operations")

  let get_out = function
    | Out oc -> Ok oc
    | In _ -> Error (`Failure "cannot use 'in_channel' for output operations")
end

module Ptr = struct
  type t = Smtml.Expr.t

  let make id = Smtml.Expr.ptr (Int32.of_int id) (Smtml.Expr.value Nothing)

  let to_int ptr =
    match Smtml.Expr.view ptr with
    | Ptr { base; _ } -> Ok (Int32.to_int base)
    | _ -> Error (`Failure "invalid channel pointer")
end

type t =
  { mutable count : int
  ; data : (int, Type.t) Hashtbl.t
  }

let make () = { count = 0; data = Hashtbl.create 16 }

let alloc tbl channel =
  let id = tbl.count in
  tbl.count <- succ tbl.count;
  Hashtbl.replace tbl.data id channel;
  Ptr.make id

let open_in tbl fpath = alloc tbl @@ In (open_in fpath)
let open_out tbl fpath = alloc tbl @@ Out (open_out fpath)

let find tbl ptr =
  let open Smtml_prelude.Result in
  let* id = Ptr.to_int ptr in
  match Hashtbl.find tbl.data id with
  | exception Not_found -> Error (`Failure "trying to use a closed channel")
  | ch -> Ok ch

let close tbl ptr =
  let open Smtml_prelude.Result in
  let* id = Ptr.to_int ptr in
  let* () =
    match Hashtbl.find tbl.data id with
    | exception Not_found -> Error (`Failure "trying to use a closed channel")
    | In ch -> Ok (close_in ch)
    | Out ch -> Ok (close_out ch)
  in
  Ok (Hashtbl.remove tbl.data id)
