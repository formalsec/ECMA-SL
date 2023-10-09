let interpreters_location : string list = Es_site.Sites.interpreters

let find file =
  List.find_opt (fun f -> String.equal f file) interpreters_location

let get_es6 () = find "es6.cesl"
