Test nodejs lazyObject:
  $ ecma-sl symbolic test_lazy_1.js
         exec : s_concat(["sausage-validator ", (`x0 : __$Str)])
  Found 1 problems!
  $ ecma-sl symbolic test_lazy_2.js
  "false"
  All Ok!
