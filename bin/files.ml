open Ecma_sl
open Syntax.Result
open Fpath

module Parser = struct
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
end

let dir_contents (recursive : bool) (dir : t) : t list Result.t =
  let fold_f fpath acc = fpath :: acc in
  let traverse = if recursive then `Any else `None in
  Result.bos (Bos.OS.Dir.fold_contents ~elements:`Files ~traverse fold_f [] dir)

let read_inputs (recursive : bool) (fpath : t) : (t * t) list Result.t =
  if is_dir_path fpath then
    let* contents = dir_contents recursive fpath in
    Ok (List.map (fun fpath' -> (fpath, fpath')) contents)
  else Ok [ (parent fpath, fpath) ]

let flat_inputs (inputs : (t * t) list list) : (t * t) list Result.t =
  let inputs' = List.flatten inputs in
  if inputs' != [] then Ok inputs'
  else Result.error (`Generic "Empty input list")

let make_subdir (dir : t) (base : t) (input : t) (outext : string) :
  t option Result.t =
  let rel_input = Option.get (relativize ~root:base input) in
  let output = (dir // rem_ext rel_input) + outext in
  match Bos.OS.Dir.create (parent output) with
  | Ok _ -> Ok (Some output)
  | Error (`Msg err) -> Result.error (`Generic err)

let make_fout (output : t option) (base : t) (input : t) (outext : string option)
  : t option Result.t =
  let outext = Option.value ~default:(get_ext input) outext in
  match output with
  | Some dir when is_dir_path dir -> make_subdir dir base input outext
  | _ -> Ok output

let exec_multiple ?(recursive : bool = true) ?(outext : string option)
  (exec_f : t -> t option -> unit Result.t) (inputs : t list) (output : t option)
  : unit Result.t =
  let open Syntax.Result in
  let process_input_f acc (base, input) =
    let* output' = make_fout output base input outext in
    match (acc, exec_f input output') with
    | (Ok (), Error err) -> Error err
    | _ -> acc
  in
  let* inputs' = list_map ~f:(read_inputs recursive) inputs in
  let* inputs'' = flat_inputs inputs' in
  List.fold_left process_input_f (Ok ()) inputs''
