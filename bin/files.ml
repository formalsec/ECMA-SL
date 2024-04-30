open Ecma_sl
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

let dir_contents (recursive : bool) (dir : t) : t list Result.t =
  let fold_f fpath acc = fpath :: acc in
  let traverse = if recursive then `Any else `None in
  Result.bos (Bos.OS.Dir.fold_contents ~elements:`Files ~traverse fold_f [] dir)

let read_dir (recursive : bool) (fpath : t) : (t * t) list Result.t =
  let open Syntax.Result in
  if is_dir_path fpath then
    let* contents = dir_contents recursive fpath in
    Ok (List.map (fun fpath' -> (parent fpath, fpath')) contents)
  else Ok [ (parent fpath, fpath) ]

let make_subdir (outdir : t) (base : t) (input : t) (outext : string) :
  t option Result.t =
  let rel_input = Option.get (relativize ~root:base input) in
  let fpath = (outdir // rem_ext rel_input) + outext in
  match Bos.OS.Dir.create (parent fpath) with
  | Ok _ -> Ok (Some fpath)
  | Error (`Msg err) -> Result.error (`Generic err)

let make_fout (output : t option) (base : t) (input : t) (outext : string) :
  t option Result.t =
  match output with
  | Some outdir when is_dir_path outdir -> make_subdir outdir base input outext
  | _ -> Ok output

let exec_multiple ?(recursive : bool = true)
  (exec_f : t -> t option -> unit Result.t) (inputs : t list)
  (output : t option) (outext : string) : unit Result.t =
  let open Syntax.Result in
  let fold_f acc (base, input) =
    let* output' = make_fout output base input outext in
    match (acc, exec_f input output') with
    | (Ok (), Error err) -> Error err
    | _ -> acc
  in
  let* inputs' = list_map ~f:(read_dir recursive) inputs in
  let inputs'' = List.flatten inputs' in
  List.fold_left fold_f (Ok ()) inputs''
