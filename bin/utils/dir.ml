let make_dir (fpath : Fpath.t) : unit =
  match Bos.OS.Dir.create fpath with
  | Ok _ -> ()
  | Error (`Msg err) -> failwith err

let dir_contents (recursive : bool) (dir : Fpath.t) : Fpath.t list =
  let traverse = if recursive then `Any else `None in
  let fold fpath acc = fpath :: acc in
  match Bos.OS.Dir.fold_contents ~elements:`Files ~traverse fold [] dir with
  | Ok fpaths -> fpaths
  | Error (`Msg err) -> failwith err

let make_fout (dir : Fpath.t) (fin : Fpath.t) (ext : string) : Fpath.t =
  let fpath = Fpath.((dir // rem_ext fin) + ext) in
  make_dir Fpath.(parent fpath);
  fpath

let exec ?(recursive : bool = true) (exec_f : Fpath.t -> Fpath.t option -> unit)
  (input : Fpath.t) (output : Fpath.t option) (outext : string) : unit =
  match (Fpath.is_dir_path input, output) with
  | (false, _) -> exec_f input output
  | (true, Some outdir) when Fpath.is_dir_path outdir ->
    let rel fpath = Option.get (Fpath.relativize ~root:input fpath) in
    let exec_f' fin = exec_f fin (Some (make_fout outdir (rel fin) outext)) in
    List.iter exec_f' (dir_contents recursive input)
  | (true, _) ->
    let exec_f' fin = exec_f fin output in
    List.iter exec_f' (dir_contents recursive input)

module Sites = struct
  let search (dirs : Fpath.t list) (file : Fpath.t) : Fpath.t option =
    List.find_map
      (fun dir ->
        let path = Fpath.(dir // file) in
        match Bos.OS.File.exists path with
        | Ok true -> Some path
        | Ok false -> None
        | Error (`Msg err) -> failwith err )
      dirs

  let resolve (path : Fpath.t) : string =
    match Bos.OS.File.read path with
    | Ok text -> String.trim text
    | Error (`Msg err) -> failwith err
end
