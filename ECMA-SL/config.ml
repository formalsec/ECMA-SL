let default_hashtbl_sz = ref 16

module Common = struct
  let warns = ref false
  let debugs = ref false
  let colored = ref true
end

module Interpreter = struct
  let hide_prints = ref false
  let debugger = ref false
  let verbose = ref false
  let verbose_at = ref false
end

module Tesl = struct
  let untyped = ref false
end

let workspace = ref "output"
