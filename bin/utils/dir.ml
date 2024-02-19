let make_base (file : string) : string =
  Filename.dirname file ^ "/" ^ Filename.basename file ^ "/"

let make_fin (base : string) (file : string) : string = base ^ file

let make_fout (base : string) (file : string) (ext : string) : string =
  base ^ Filename.remove_extension file ^ ext

let exec (exec_f : string -> string option -> unit) (input : string)
  (output : string option) (outext : string) : unit =
  match (Filename.extension input, output) with
  | ("", Some output') when Filename.extension output' = "" ->
    let base_in = make_base input in
    let base_out = make_base output' in
    let make_fin' fin = make_fin base_in fin in
    let make_fout' fin = Some (make_fout base_out fin outext) in
    let exec_f' fin = exec_f (make_fin' fin) (make_fout' fin) in
    Ecma_sl.Io.safe_mkdir base_out;
    Array.iter exec_f' (Sys.readdir input)
  | ("", _) ->
    let base_in = make_base input in
    let make_fin' fin = make_fin base_in fin in
    let exec_f' fin = exec_f (make_fin' fin) output in
    Array.iter exec_f' (Sys.readdir input)
  | _ -> exec_f input output
