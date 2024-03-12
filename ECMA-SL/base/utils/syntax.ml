module Option = struct
  let ( let* ) v f = Option.bind v f
  let ( let+ ) v f = Option.map f v
end

module Result = struct
  let ( let* ) v f = Result.bind v f
  let ( let+ ) v f = Result.map f v

  (* Taken from: https://github.com/OCamlPro/owi/blob/main/src/syntax.ml *)
  let list_map ~f l =
    let exit = ref None in
    let exception Exit in
    try
      Ok
        (List.map
           (fun v ->
             match f v with
             | Error err ->
               exit := Some err;
               raise Exit
             | Ok v -> v )
           l )
    with Exit -> Error (Stdlib.Option.get !exit)

  let list_filter_map ~f l =
    let exit = ref None in
    let exception Exit in
    try
      Ok
        (List.filter_map
           (fun v ->
             match f v with
             | Error err ->
               exit := Some err;
               raise Exit
             | Ok v -> v )
           l )
    with Exit -> Error (Stdlib.Option.get !exit)
end
