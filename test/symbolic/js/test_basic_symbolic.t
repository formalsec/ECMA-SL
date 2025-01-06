Test basic symbolic number:
  $ ecma-sl symbolic basic_symbolic_number.js
  - : app = { "target": symbol("empty"), "type": symbol("normal"),
              "__completion__": true, "value": 0,  }
  All Ok!
  $ ecma-sl symbolic basic_symbolic_string.js
  - : app = { "target": symbol("empty"), "type": symbol("normal"),
              "__completion__": true, "value": 0,  }
  All Ok!
  $ ecma-sl symbolic symbolic_number_arith.js
  - : app = { "target": symbol("empty"), "type": symbol("normal"),
              "__completion__": true, "value": 0,  }
  All Ok!
  $ ecma-sl symbolic symbolic_number_branching.js
  - : app = { "target": symbol("empty"), "type": symbol("normal"),
              "__completion__": true, "value": 0,  }
  - : app = { "target": symbol("empty"), "type": symbol("normal"),
              "__completion__": true, "value": 0,  }
  - : app = { "target": symbol("empty"), "type": symbol("normal"),
              "__completion__": true, "value": 0,  }
  All Ok!
  $ ecma-sl symbolic symbolic_string_operations.js
  - : app = { "target": symbol("empty"), "type": symbol("normal"),
              "__completion__": true, "value": 0,  }
  All Ok!
  $ ecma-sl symbolic symbolic_string_branching.js
  - : app = { "target": symbol("empty"), "type": symbol("normal"),
              "__completion__": true, "value": 0,  }
  - : app = { "target": symbol("empty"), "type": symbol("normal"),
              "__completion__": true, "value": 0,  }
  All Ok!
  $ ecma-sl symbolic symbolic_string_array.js
  (str.++ ((str.++ (flour, " ")), water))
       assert : failure with (false)
  Found 1 problems!
  [21]
  $ ecma-sl symbolic node_require_a.js
  "B"
  "A"
  - : app = { "target": symbol("empty"), "__completion__": true,
              "value": symbol("null"), "type": symbol("normal"),  }
  All Ok!
  $ ecma-sl symbolic node_require_constant_a.js
  "sausage"
  - : app = { "target": symbol("empty"), "__completion__": true,
              "value": symbol("null"), "type": symbol("normal"),  }
  All Ok!
  $ ecma-sl symbolic node_require_function_a.js
  "In node_require_function_a.js"
  "chourico"
  - : app = { "target": symbol("empty"), "type": symbol("normal"),
              "__completion__": true, "value": symbol("null"),  }
  All Ok!
  $ ecma-sl symbolic node_require_modify_global_a.js
  "polluted"
  - : app = { "target": symbol("empty"), "__completion__": true,
              "value": symbol("null"), "type": symbol("normal"),  }
  All Ok!
