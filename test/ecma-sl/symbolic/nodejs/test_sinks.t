Test Function:
  $ ecma-sl symbolic test_sink_Function.js
         eval : (`body : __$Str)
  Found 1 problems!

Test argv:
  $ ecma-sl symbolic test_sink_argv.js
  (`argv2 : __$Str)
  All Ok!

Test eval:
  $ ecma-sl symbolic test_sink_eval.js
         eval : (`source : __$Str)
  Found 1 problems!

Test exec:
  $ ecma-sl symbolic test_sink_exec.js
         exec : s_concat(["git fetch ", (`remote : __$Str)])
  Found 1 problems!

Test fs:
  $ ecma-sl symbolic test_sink_fs.js
     readFile : (`source : __$Str)
  Found 1 problems!
