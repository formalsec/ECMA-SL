let default_hashtbl_sz = ref 16

module Common = struct
  let colored = ref true
end

module Eslerr = struct
  let show_code = ref true
end

module Tesl = struct
  let untyped = ref false
end

let file = ref ""
let workspace = ref "output"
