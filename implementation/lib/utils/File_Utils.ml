let burn_to_disk (path : string) (data : string) : unit =
  let oc = open_out path in
  output_string oc data;
  close_out oc
