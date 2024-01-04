module Option = struct
  let ( let* ) v f = Option.bind v f
  let ( let+ ) v f = Option.map f v
  let map_default f d = function None -> d | Some v' -> f v'
end

module Result = struct
  let ( let* ) v f = Result.bind v f
  let ( let+ ) v f = Result.map f v

  let list_map ~f l =
    let exception E of string in
    try
      Ok
        (List.map
           (fun v -> match f v with Error s -> raise (E s) | Ok v -> v)
           l )
    with E s -> Error s
end
