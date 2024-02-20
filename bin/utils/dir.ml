let make_dir (fpath : Fpath.t) : unit =
  match Bos.OS.Dir.create fpath with
  | Ok _ -> ()
  | Error (`Msg err) -> failwith err

let dir_contents (fpath : Fpath.t) : Fpath.t list =
  match Bos.OS.Dir.contents fpath with
  | Ok files -> files
  | Error (`Msg err) -> failwith err

let make_fout (dir : Fpath.t) (fin : Fpath.t) (ext : string) : Fpath.t =
  Fpath.((dir / filename fin) + ext)

let exec (exec_f : Fpath.t -> Fpath.t option -> unit) (input : Fpath.t)
  (output : Fpath.t option) (outext : string) : unit =
  match (Fpath.is_dir_path input, output) with
  | (false, _) -> exec_f input output
  | (true, Some outdir) when Fpath.is_dir_path outdir ->
    make_dir outdir;
    let exec_f' fin = exec_f fin (Some (make_fout outdir fin outext)) in
    List.iter exec_f' (dir_contents input)
  | (true, _) ->
    let exec_f' fin = exec_f fin output in
    List.iter exec_f' (dir_contents input)
