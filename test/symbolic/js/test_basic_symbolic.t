Test basic symbolic number:
  $ ecma-sl symbolic basic_symbolic_number.js
  - : app = { "type": symbol("normal"), "value": 0, "__completion__": true,
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic basic_symbolic_string.js
  - : app = { "target": symbol("empty"), "type": symbol("normal"), "value": 0,
              "__completion__": true,  }
  All Ok!
  $ ecma-sl symbolic symbolic_number_arith.js
  - : app = { "type": symbol("normal"), "value": 0, "__completion__": true,
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic symbolic_number_branching.js
  - : app = { "type": symbol("normal"), "value": 0, "__completion__": true,
              "target": symbol("empty"),  }
  - : app = { "type": symbol("normal"), "value": 0, "__completion__": true,
              "target": symbol("empty"),  }
  - : app = { "type": symbol("normal"), "value": 0, "__completion__": true,
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic symbolic_string_operations.js
  - : app = { "type": symbol("normal"), "value": 0, "__completion__": true,
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic symbolic_string_branching.js
  - : app = { "type": symbol("normal"), "__completion__": true, "value": 0,
              "target": symbol("empty"),  }
  - : app = { "type": symbol("normal"), "__completion__": true, "value": 0,
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic symbolic_string_array.js
  (str.++ ((str.++ (flour, " ")), water))
       assert : failure with (false)
  Found 1 problems!
  [21]
  $ ecma-sl symbolic node_require_a.js
  "B"
  "A"
  - : app = { "type": symbol("normal"), "target": symbol("empty"),
              "__completion__": true, "value": symbol("null"),  }
  All Ok!
  $ ecma-sl symbolic node_require_constant_a.js
  "sausage"
  - : app = { "type": symbol("normal"), "value": symbol("null"),
              "target": symbol("empty"), "__completion__": true,  }
  All Ok!
  $ ecma-sl symbolic node_require_function_a.js
  "In node_require_function_a.js"
  "chourico"
  - : app = { "type": symbol("normal"), "value": symbol("null"),
              "__completion__": true, "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic node_require_modify_intrinsics_a.js
  "polluted"
  - : app = { "type": symbol("normal"), "__completion__": true,
              "target": symbol("empty"), "value": symbol("null"),  }
  All Ok!
