module Locations = struct
  let interps : string list = Site.Sites.interpreters
  let stdlib : string list = Site.Sites.stdlib
end

let stdlib = List.hd Locations.stdlib

let search (location : string list) (file : string) : string option =
  List.find_map
    (fun dir ->
      let path = Filename.concat dir file in
      if Sys.file_exists path then Some path else None )
    location

let resolve (location : string list) (file : string) : string =
  search location file |> Option.get |> EslBase.Io.read_file |> String.trim

let es5_config () : string = resolve Locations.interps "es5.include"
let es6_config () : string = resolve Locations.interps "es6.include"
let es6_sym_config () : string = resolve Locations.interps "es6-sym.include"
let es6_sym_interp () : string = resolve Locations.interps "es6-sym.cesl"
