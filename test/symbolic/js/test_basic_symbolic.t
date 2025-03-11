Test basic symbolic number:
  $ ecma-sl symbolic basic_symbolic_number.js
  - : app = { "value": 0, "type": symbol("normal"), "__completion__": true,
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic basic_symbolic_string.js
  - : app = { "value": 0, "type": symbol("normal"), "target": symbol("empty"),
              "__completion__": true,  }
  All Ok!
  $ ecma-sl symbolic symbolic_number_arith.js
  - : app = { "value": 0, "type": symbol("normal"), "target": symbol("empty"),
              "__completion__": true,  }
  All Ok!
  $ ecma-sl symbolic symbolic_number_branching.js
  - : app = { "value": 0, "type": symbol("normal"), "target": symbol("empty"),
              "__completion__": true,  }
  - : app = { "value": 0, "type": symbol("normal"), "target": symbol("empty"),
              "__completion__": true,  }
  - : app = { "value": 0, "type": symbol("normal"), "target": symbol("empty"),
              "__completion__": true,  }
  All Ok!
  $ ecma-sl symbolic symbolic_string_operations.js
  - : app = { "target": symbol("empty"), "value": 0, "type": symbol("normal"),
              "__completion__": true,  }
  All Ok!
  $ ecma-sl symbolic symbolic_string_branching.js
  - : app = { "value": 0, "__completion__": true, "type": symbol("normal"),
              "target": symbol("empty"),  }
  - : app = { "value": 0, "__completion__": true, "type": symbol("normal"),
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic symbolic_string_array.js
  "":82957.2-82957.20: Assert failure:
   Stmt: assert (hd params)
   Expr: false
  Path condition:
   (bool.eq "banana bread" (str.++ ((str.++ (flour, " ")), water)))
  Model:
   (model
     (flour str "banana")
     (water str "bread"))
  Found 1 problems!
  [21]
