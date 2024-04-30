module Option = struct
  include Option

  let ( let* ) v f = bind v f
  let ( let+ ) v f = map f v
end

module Result = struct
  include Result

  let ( let* ) v f = bind v f
  let ( let+ ) v f = map f v

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
