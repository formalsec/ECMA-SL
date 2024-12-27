Test basic symbolic number:
  $ ecma-sl symbolic basic_symbolic_number.js
  - : app = loc(8055)
  All Ok!
  $ ecma-sl symbolic basic_symbolic_string.js
  - : app = loc(8055)
  All Ok!
  $ ecma-sl symbolic symbolic_number_arith.js
  - : app = loc(8093)
  All Ok!
  $ ecma-sl symbolic symbolic_number_branching.js
  - : app = loc(8034)
  - : app = loc(8042)
  - : app = loc(8050)
  All Ok!
  $ ecma-sl symbolic symbolic_string_operations.js
  - : app = loc(8072)
  All Ok!
  $ ecma-sl symbolic symbolic_string_branching.js
  - : app = loc(8034)
  - : app = loc(8042)
  All Ok!
