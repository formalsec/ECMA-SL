let name = "js2cesl"
let version = "v0.1"
let banner () = print_endline (name ^ " " ^ version)
let usage = "Usage: " ^ name ^ " [option] [file ...]"
let file = ref ""
let output = ref ""
let interp = ref "share/interpreter/es6.cesl"

let argspec =
  Arg.align
    [
      ("-i", Arg.Set_string file, " read program from file");
      ("-o", Arg.Set_string output, " write result to file");
      ("-interp", Arg.Set_string interp, " path to javascript interpreter");
      ( "-v",
        Arg.Unit
          (fun () ->
            banner ();
            exit 0),
        " show version" );
      ("-verb", Arg.Set Config.verbose, " verbose");
    ]

let compile file interp out_file =
  let ast_file = Filename.remove_extension file ^ "_ast.cesl" in
  let ret =
    Sys.command
      (String.concat " " [ "js2ecma-sl"; "-c"; "-i"; file; "-o"; ast_file ])
  in
  if ret <> 0 then prerr_endline "Unable to compile JS program!"
  else
    let ast_str = Core.In_channel.read_all ast_file
    and interp_str = Core.In_channel.read_all interp in
    let program = String.concat ";\n" [ ast_str; interp_str ] in
    Core.Out_channel.write_all out_file ~data:program;
    Sys.remove ast_file

let () =
  Printexc.record_backtrace true;
  try
    Arg.parse argspec (fun f -> file := f) usage;
    if !file = "" then (
      Arg.usage argspec usage;
      exit 2)
    else if not (Sys.file_exists !file) then (
      prerr_endline ("Unable to find \"" ^ !file ^ "\"");
      exit 2)
    else if not (Sys.file_exists !interp) then (
      prerr_endline ("Unable to find \"" ^ !interp ^ "\"");
      exit 2)
    else
      let out_file =
        if !output <> "" then !output
        else Filename.remove_extension !file ^ ".cesl"
      in
      compile !file !interp out_file
  with exn ->
    flush_all ();
    prerr_endline
      (Sys.argv.(0) ^ ": unknown exception " ^ Printexc.to_string exn);
    exit 2
