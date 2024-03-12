module Location = struct
  let interpreters : string list = Site.Sites.interpreters
  let nodejs : string list = Site.Sites.nodejs
end

let search_file (location : string list) (file : string) : string option =
  List.find_map
    (fun dir ->
      let filename = Filename.concat dir file in
      if Sys.file_exists filename then Some filename else None )
    location

let resolve_config (file : string) : string =
  String.trim (Ecma_sl.Io.read_file file)

let es5_config () = search_file Location.interpreters "es5.include"
let es6_config () = search_file Location.interpreters "es6.include"
let es6_sym_config () = search_file Location.interpreters "es6-sym.include"
let es6_interp () = search_file Location.interpreters "es6-sym.cesl"
let esl_symbolic_config () = search_file Location.nodejs "esl_symbolic.js"
