Test Function:
  $ ecma-sl symbolic test_sink_Function.js
        abort : #body
  Found 1 problems!

Test argv:
  $ ecma-sl symbolic test_sink_argv.js
  #argv2
  All Ok!

Test eval:
  $ ecma-sl symbolic test_sink_eval.js
        abort : #source
  Found 1 problems!

Test exec:
  $ ecma-sl symbolic test_sink_exec.js
        abort : #source
  Found 1 problems!

Test fs:
  $ ecma-sl symbolic test_sink_fs.js
        abort : #source
  Found 1 problems!
