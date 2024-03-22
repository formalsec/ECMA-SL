module Option = struct
  let ( let* ) v f = Option.bind v f
  let ( let+ ) v f = Option.map f v
end

module Result = struct
  let ( let* ) v f = Result.bind v f
  let ( let+ ) v f = Result.map f v

  let rec list_iter ~f = function
    | [] -> Ok ()
    | hd :: tl -> (
      match f hd with Error _ as err -> err | Ok () -> list_iter ~f tl )

  let rec list_map ~f = function
    | [] -> Ok []
    | hd :: tl -> (
      match f hd with
      | Error _ as err -> err
      | Ok v ->
        let+ tl = list_map ~f tl in
        v :: tl )

  let list_filter_map ~f list =
    let rec aux acc = function
      | [] -> Ok (List.rev acc)
      | hd :: tl -> (
        match f hd with
        | Error _ as err -> err
        | Ok None -> aux acc tl
        | Ok (Some v) -> aux (v :: acc) tl )
    in
    aux [] list
end
