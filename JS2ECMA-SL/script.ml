let () =
  if Array.length Sys.argv < 2 then
    Format.ksprintf failwith "Usage: %s <js script>@." Sys.argv.(0)

let set_npm_bin () =
  let in_chan = Unix.open_process_in "npm config get prefix" in
  let prefix =
    Fun.protect
      ~finally:(fun () -> close_in in_chan)
      (fun () -> input_line in_chan)
  in
  let path = Unix.getenv "PATH" in
  Format.ksprintf (Unix.putenv "PATH") "%s/bin:%s" prefix path

let () =
  let code = Sys.command "npm -v > /dev/null 2>&1" in
  if code <> 0 then begin
    Format.eprintf "Please install 'npm'@\n";
    exit code
  end;
  set_npm_bin ();
  let pkg_v = "pkg -v > /dev/null 2>&1" in
  let code = Sys.command pkg_v in
  if code <> 0 then begin
    let code = Sys.command "npm install -g pkg > /dev/null 2>&1" in
    if code <> 0 then begin
      Format.eprintf "Failed to install 'pkg'@\n";
      exit code
    end;
    assert (Sys.command pkg_v = 0)
  end

let () =
  let code = Sys.command "npm install > /dev/null 2>&1" in
  if code <> 0 then begin
    Format.eprintf "Failed to install project dependencies@\n";
    exit code
  end;
  let js_program = Sys.argv.(1) in
  let target = "node18" in
  let code =
    Format.ksprintf Sys.command "pkg %s -t %s -o js2ecma-sl > /dev/null 2>&1"
      js_program target
  in
  if code <> 0 then begin
    Format.eprintf "Failed to compile 'j2ecma-sl'@\n";
    exit code
  end;
  exit 0
