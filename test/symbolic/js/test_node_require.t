  $ ecma-sl symbolic node_require_a.js
  "B"
  "A"
  - : app = { "__completion__": true, "value": symbol("null"),
              "target": symbol("empty"), "type": symbol("normal"),  }
  All Ok!
  $ ecma-sl symbolic node_require_constant_a.js
  "sausage"
  - : app = { "__completion__": true, "value": symbol("null"),
              "target": symbol("empty"), "type": symbol("normal"),  }
  All Ok!
  $ ecma-sl symbolic node_require_function_a.js
  "In node_require_function_a.js"
  "chourico"
  - : app = { "__completion__": true, "target": symbol("empty"),
              "value": symbol("null"), "type": symbol("normal"),  }
  All Ok!
  $ ecma-sl symbolic node_require_modify_intrinsics_a.js
  "polluted"
  - : app = { "target": symbol("empty"), "value": symbol("null"),
              "type": symbol("normal"), "__completion__": true,  }
  All Ok!
  $ ecma-sl symbolic ./relative-require/a.js
  "a"
  "b"
  "c"
  - : app = { "target": symbol("empty"), "value": symbol("null"),
              "__completion__": true, "type": symbol("normal"),  }
  All Ok!
