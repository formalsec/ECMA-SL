let safe_mkdir (dir : string) : unit =
  if not (Sys.file_exists dir) then ignore (Sys.command ("mkdir -p " ^ dir))

let write_file (file : string) (data : string) : unit =
  let oc = open_out file in
  output_string oc data;
  close_out oc

let load_file (file : string) : string =
  let ic = open_in file in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s
