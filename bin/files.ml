open Fpath

let parse_fpath (path : string)
  (test_f : t -> (bool, [< `Msg of string ]) result) :
  [> `Error of string | `Ok of t ] =
  let fpath = v path in
  match test_f fpath with
  | Ok true -> `Ok fpath
  | Ok false -> `Error (Format.asprintf "File '%s' not found!" path)
  | Error (`Msg err) -> `Error err

let fpath = ((fun str -> `Ok (v str)), pp)
let valid_fpath = ((fun str -> parse_fpath str Bos.OS.Path.exists), pp)
let non_dir_fpath = ((fun str -> parse_fpath str Bos.OS.File.exists), pp)
let dir_fpath = ((fun str -> parse_fpath str Bos.OS.Dir.exists), pp)

let make_dir (fpath : t) : unit =
  match Bos.OS.Dir.create fpath with
  | Error (`Msg err) -> EslBase.Log.fail "%s" err
  | Ok _ -> ()

let dir_contents (recursive : bool) (dir : t) : t list =
  let fold_f fpath acc = fpath :: acc in
  let traverse = if recursive then `Any else `None in
  match Bos.OS.Dir.fold_contents ~elements:`Files ~traverse fold_f [] dir with
  | Error (`Msg err) -> EslBase.Log.fail "%s" err
  | Ok fpaths -> fpaths

let make_fout (dir : t) (fin : t) (outext : string) : t =
  let fpath = (dir // rem_ext fin) + outext in
  make_dir (parent fpath);
  fpath

let exec ?(recursive : bool = true) (exec_f : t -> t option -> unit) (input : t)
  (output : t option) (outext : string) : unit =
  match (is_dir_path input, output) with
  | (false, _) -> exec_f input output
  | (true, Some outdir) when is_dir_path outdir ->
    let rel fpath = Option.get (relativize ~root:input fpath) in
    let exec_f' fin = exec_f fin (Some (make_fout outdir (rel fin) outext)) in
    List.iter exec_f' (dir_contents recursive input)
  | (true, _) ->
    let exec_f' fin = exec_f fin output in
    List.iter exec_f' (dir_contents recursive input)

let exec_multiple ?(recursive : bool = true) (exec_f : t -> t option -> unit)
  (inputs : t list) (output : t option) (outext : string) : unit =
  List.iter (fun input -> exec ~recursive exec_f input output outext) inputs
