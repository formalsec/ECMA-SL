No pollution:
  $ ecma-sl symbolic pollution_1.js
Test pollution with depth = 3
  $ ecma-sl symbolic pollution_2.js
        abort : "Prototype pollution detected!"
Test pollution with depth = 2
  $ ecma-sl symbolic pollution_3.js
        abort : "Prototype pollution detected!"
