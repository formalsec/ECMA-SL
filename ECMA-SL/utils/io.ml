let safe_mkdir (dir : string) : unit =
  if not (Sys.file_exists dir) then ignore (Sys.command ("mkdir -p " ^ dir))

let write_file ~(file : string) ~(data : string) : unit =
  let oc = open_out file in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc data)

let load_file (file : string) : string =
  let ic = open_in file in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n;
      Bytes.to_string s )
