
let%test "test_is_empty" = S_object2.create () |> S_object2.is_empty

(*
let%test "test_set_val" =
  let k = Val (Str "k") and d = Val (Int 0) in
  S_object2.create () |> S_object2.set ~k ~d 

let%test "test_set_symbolic" =
  let k = Symbolic (StrType, Val (Str "x")) and d = Val (Int 3) in
  S_object2.create () |> S_object2.set ~k ~d |> List.length |> Int.equal 1

let%test "test_set_val_symbolic" =
  let k0 = Val (Str "k")
  and d0 = Val (Int 0)
  and k1 = Symbolic (StrType, Val (Str "x"))
  and d1 = Val (Int 1) in
  S_object2.create () |> S_object2.set ~k:k0 ~d:d0 |> List.hd
  |> S_object2.set ~k:k1 ~d:d1 |> List.length |> Int.equal 2

let%test "test_get_empty" =
  let k = Val (Str "k") in
  let o = S_object2.create () in
  S_object2.get o k |> List.length |> Int.equal 0
  *)
