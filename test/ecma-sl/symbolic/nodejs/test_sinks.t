Test Function:
  $ ecma-sl symbolic test_sink_Function.js
        abort : #body

Test argv:
  $ ecma-sl symbolic test_sink_argv.js
  #argv2

Test eval:
  $ ecma-sl symbolic test_sink_eval.js
        abort : #source

Test exec:
  $ ecma-sl symbolic test_sink_exec.js
        abort : #source

Test fs:
  $ ecma-sl symbolic test_sink_fs.js
        abort : #source
