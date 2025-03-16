let read_channel (ic : in_channel) : string =
  let rec loop acc =
    try
      let line = input_line ic in
      loop (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  loop ""

let fin (process_f : in_channel -> 'a) (file : string) : 'a =
  let ic = open_in file in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> process_f ic)

let fout (process_f : out_channel -> 'a) (file : string) : 'a =
  let oc = open_out file in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> process_f oc)

let read_in_channel (ic : in_channel) : string =
  let in_sz = in_channel_length ic in
  really_input_string ic in_sz

let write_out_channel (data : string) (oc : out_channel) : unit =
  output_string oc data

let read_file (file : string) : string = fin read_in_channel file

let write_file (file : string) (data : string) : unit =
  fout (write_out_channel data) file
