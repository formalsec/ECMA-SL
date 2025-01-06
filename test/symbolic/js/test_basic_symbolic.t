Test basic symbolic number:
  $ ecma-sl symbolic basic_symbolic_number.js
  - : app = { "__completion__": true, "value": 0, "type": symbol("normal"),
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic basic_symbolic_string.js
  - : app = { "__completion__": true, "value": 0, "type": symbol("normal"),
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic symbolic_number_arith.js
  - : app = { "target": symbol("empty"), "value": 0, "__completion__": true,
              "type": symbol("normal"),  }
  All Ok!
  $ ecma-sl symbolic symbolic_number_branching.js
  - : app = { "__completion__": true, "value": 0, "type": symbol("normal"),
              "target": symbol("empty"),  }
  - : app = { "__completion__": true, "value": 0, "type": symbol("normal"),
              "target": symbol("empty"),  }
  - : app = { "__completion__": true, "value": 0, "type": symbol("normal"),
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic symbolic_string_operations.js
  - : app = { "__completion__": true, "value": 0, "target": symbol("empty"),
              "type": symbol("normal"),  }
  All Ok!
  $ ecma-sl symbolic symbolic_string_branching.js
  - : app = { "value": 0, "type": symbol("normal"), "target": symbol("empty"),
              "__completion__": true,  }
  - : app = { "value": 0, "type": symbol("normal"), "target": symbol("empty"),
              "__completion__": true,  }
  All Ok!
  $ ecma-sl symbolic symbolic_string_array.js
  (str.++ ((str.++ (flour, " ")), water))
       assert : failure with (false)
  Found 1 problems!
  [21]
  $ ecma-sl symbolic node_require_a.js
  "B"
  "A"
  - : app = { "target": symbol("empty"), "value": symbol("null"),
              "type": symbol("normal"), "__completion__": true,  }
  All Ok!
  $ ecma-sl symbolic node_require_constant_a.js
  "sausage"
  - : app = { "value": symbol("null"), "target": symbol("empty"),
              "__completion__": true, "type": symbol("normal"),  }
  All Ok!
  $ ecma-sl symbolic node_require_function_a.js
  "In node_require_function_a.js"
  "chourico"
  - : app = { "__completion__": true, "value": symbol("null"),
              "type": symbol("normal"), "target": symbol("empty"),  }
  All Ok!
