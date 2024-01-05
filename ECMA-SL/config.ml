let file = ref ""
let target = ref "main"
let workspace = ref "output"
let default_hashtbl_sz = ref 16

module Common = struct
  let colored = ref true
end

module Eslerr = struct
  let show_code = ref true
end
