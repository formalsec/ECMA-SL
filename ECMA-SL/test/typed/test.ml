let syntax_succ (file : string) : unit =
  let data = Parsing_utils.load_file file in
  ignore (Parsing_utils.parse_e_prog file data)