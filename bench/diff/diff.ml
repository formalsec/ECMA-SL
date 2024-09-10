let debug = false
let debug fmt k = if debug then k (Format.eprintf fmt)

type status =
  [ `SUCCESS
  | `FAILURE
  | `ANOMALY
  ]

module Fmap = Fpath.Map

type diff =
  { orig : status
  ; new_ : status
  }

let pp_status fmt = function
  | `SUCCESS -> Format.pp_print_string fmt "SUCCESS"
  | `FAILURE -> Format.pp_print_string fmt "FAILURE"
  | `ANOMALY -> Format.pp_print_string fmt "ANOMALY"

let parse_results file =
  match Bos.OS.File.read_lines file with
  | Error (`Msg err) -> Fmt.failwith "%s" err
  | Ok lines ->
    List.fold_left
      (fun map line ->
        match String.split_on_char ' ' (String.trim line) with
        | [ test; _; "SUCCESS"; _ ] -> Fmap.add (Fpath.v test) `SUCCESS map
        | [ test; _; "FAILURE"; _ ] -> Fmap.add (Fpath.v test) `FAILURE map
        | [ test; _; "ANOMALY"; _ ] -> Fmap.add (Fpath.v test) `ANOMALY map
        (* Ignore the other lines *)
        | _ -> map )
      Fmap.empty lines

let diff orig new_ =
  debug "Files: %a %a@." (fun k -> k Fpath.pp orig Fpath.pp new_);
  let orig_map = parse_results orig in
  let new_map = parse_results new_ in
  let diff =
    Fmap.merge
      (fun _ orig new_ ->
        match (orig, new_) with
        | (Some `SUCCESS, Some `SUCCESS)
        | (Some `FAILURE, Some `FAILURE)
        | (Some `ANOMALY, Some `ANOMALY) ->
          None
        | (Some orig, Some new_) ->
          (* Result changed *)
          Some { orig; new_ }
        | ((None | Some _), _) ->
          (* Should never happen *)
          assert false )
      orig_map new_map
  in
  (* Sorry *)
  Format.printf "%a@."
    (Format.pp_print_iter
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;")
       (fun f tbl -> Fmap.iter (fun p status -> f (p, status)) tbl)
       (fun fmt (p, { orig; new_ }) ->
         Format.fprintf fmt "%a: %a -> %a" Fpath.pp p pp_status orig pp_status
           new_ ) )
    diff

let cli =
  let open Cmdliner in
  let fpath = ((fun str -> `Ok (Fpath.v str)), Fpath.pp) in
  let file0 = Arg.(required & pos 0 (some fpath) None & info []) in
  let file1 = Arg.(required & pos 1 (some fpath) None & info []) in
  let doc = "Diff tool for test262 output comparison" in
  let info = Cmd.info ~doc ~version:"%%VERSION%%" "diff" in
  Cmd.v info Term.(const diff $ file0 $ file1)

let () =
  let open Cmdliner in
  match Cmd.eval_value cli with
  | Ok (`Help | `Ok () | `Version) -> exit Cmd.Exit.ok
  | Error `Parse -> exit Cmd.Exit.cli_error
  | Error (`Exn | `Term) -> exit Cmd.Exit.internal_error
