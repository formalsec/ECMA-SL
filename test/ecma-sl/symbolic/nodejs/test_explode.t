Test eval explode :
  $ ecma-sl explode-js test_sink_eval.js
        abort : #source
  Found 1 problems!
    replaying : test_sink_eval.js...
      running : ecma-out/test-suite/witness-0.js
       status : true ("success" in output)
Test exec explode:
  $ ecma-sl explode-js test_sink_exec.js
        abort : #source
  Found 1 problems!
    replaying : test_sink_exec.js...
      running : ecma-out/test-suite/witness-0.js
       status : false (no side effect)
