  $ ecma-sl symbolic node_require_a.js
  "B"
  "A"
  - : app = { "target": symbol("empty"), "type": symbol("normal"),
              "__completion__": true, "value": symbol("null"),  }
  All Ok!
  $ ecma-sl symbolic node_require_constant_a.js
  "sausage"
  - : app = { "value": symbol("null"), "type": symbol("normal"),
              "__completion__": true, "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic node_require_function_a.js
  "In node_require_function_a.js"
  "chourico"
  - : app = { "value": symbol("null"), "__completion__": true,
              "type": symbol("normal"), "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic node_require_modify_intrinsics_a.js
  "polluted"
  - : app = { "type": symbol("normal"), "__completion__": true,
              "target": symbol("empty"), "value": symbol("null"),  }
  All Ok!
  $ ecma-sl symbolic ./relative-require/a.js
  "a"
  "b"
  "c"
  - : app = { "value": symbol("null"), "__completion__": true,
              "type": symbol("normal"), "target": symbol("empty"),  }
  All Ok!
