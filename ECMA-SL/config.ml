let default_hashtbl_sz = ref 16

module Interpreter = struct
  let debugger = ref false
  let verbose = ref false
  let verbose_at = ref false
end

module Tesl = struct
  let untyped = ref false
end

let workspace = ref "output"
