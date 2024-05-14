open Ecma_sl
open Syntax.Result
open Fpath

module Parser = struct
  let parse (str : string) (test_f : t -> (bool, [< `Msg of string ]) result) :
    [> `Ok of t | `Error of string ] =
    let fpath = v str in
    match test_f fpath with
    | Ok true -> `Ok fpath
    | Ok false -> `Error (Format.asprintf "File '%s' not found!" str)
    | Error (`Msg err) -> `Error err

  let fix_dir (fpath : [> `Ok of t | `Error of string ]) :
    [> `Ok of t | `Error of string ] =
    match fpath with
    | `Ok fpath' -> (
      match Bos.OS.Dir.exists fpath' with
      | Ok true -> `Ok (to_dir_path fpath')
      | Ok false -> `Ok fpath'
      | Error (`Msg err) -> `Error err )
    | `Error _ as err -> err

  let fpath = ((fun str -> `Ok (v str)), pp)
  let valid_fpath = ((fun str -> parse str Bos.OS.Path.exists |> fix_dir), pp)
  let non_dir_fpath = ((fun str -> parse str Bos.OS.File.exists), pp)
  let dir_fpath = ((fun str -> parse str Bos.OS.Dir.exists), pp)
end

type output =
  [ `None
  | `Fixed of t
  | `Generated of t
  ]

let get_output (output : output) : t option =
  match output with
  | `None -> None
  | `Fixed output' -> Some output'
  | `Generated output' -> Some output'

let dir_contents (recursive : bool) (dir : t) : t list Result.t =
  let fold_f fpath acc = fpath :: acc in
  let traverse = if recursive then `Any else `None in
  Result.bos (Bos.OS.Dir.fold_contents ~elements:`Files ~traverse fold_f [] dir)

let read_inputs (recursive : bool) (input : t) : (t * t) list Result.t =
  if is_dir_path input then
    let* contents = dir_contents recursive input in
    Ok (List.map (fun fpath' -> (input, fpath')) contents)
  else Ok [ (parent input, input) ]

let flat_inputs (inputs : (t * t) list list) : (t * t) list Result.t =
  let inputs' = List.flatten inputs in
  if inputs' != [] then Ok inputs'
  else Result.error (`Generic "Empty input list")

let make_subdir (dir : t) (workspace : t) (input : t) (outext : string) :
  output Result.t =
  let rel_input = Option.get (relativize ~root:workspace input) in
  let output = (dir // rem_ext rel_input) + outext in
  match Bos.OS.Dir.create (parent output) with
  | Ok _ -> Ok (`Generated output)
  | Error (`Msg err) -> Result.error (`Generic err)

let make_fout (output : t option) (workspace : t) (input : t)
  (outext : string option) : output Result.t =
  let outext = Option.value ~default:(get_ext input) outext in
  match output with
  | Some dir when is_dir_path dir -> make_subdir dir workspace input outext
  | Some output' -> Ok (`Fixed output')
  | None -> Ok `None

let generate_input_list ?(recursive : bool = true) (inputs : t list) :
  (t * t) list Result.t =
  let* inputs' = list_map ~f:(read_inputs recursive) inputs in
  flat_inputs inputs'

let process_inputs ?(outext : string option)
  (exec_f : t -> t -> output -> unit Result.t) (inputs : (t * t) list)
  (output : t option) : unit Result.t =
  let process_input_f acc (workspace, input) =
    let* output' = make_fout output workspace input outext in
    match (acc, exec_f workspace input output') with
    | (Ok (), (Error _ as err)) -> err
    | _ -> acc
  in
  List.fold_left process_input_f (Ok ()) inputs
