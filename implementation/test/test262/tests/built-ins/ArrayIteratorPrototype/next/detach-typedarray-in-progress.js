// Copyright (C) 2016 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
esid: sec-%arrayiteratorprototype%.next
description: If the underlying TypedArray is detached during iteration, throw
info: |
  %ArrayIteratorPrototype%.next( )

  ...
  8. If _a_ has a [[TypedArrayName]] internal slot, then
    a. If IsDetachedBuffer(_a_.[[ViewedArrayBuffer]]) is *true*, throw a *TypeError* exception.
includes: [testTypedArray.js, detachArrayBuffer.js]
features: [TypedArray]
---*/

testWithTypedArrayConstructors(function (TA) {/* TA => { TODO: Uncomment and delete call below when arrow functions are implemented */
  var typedArray = new TA(5);
  var i = 0;
  /* assert.throws(TypeError, () => { TODO: Uncomment and delete call below when arrow functions are implemented
    for (let key of typedArray.keys()) {
      $DETACHBUFFER(typedArray.buffer);
      i++;
    }
  }); */
  assert.throws(TypeError, function () {
    for (let key of typedArray.keys()) { /* No for-of operator in ESL_interpreter.esl */
      $DETACHBUFFER(typedArray.buffer);
      i++;
    }
  });
  assert.sameValue(i, 1);
});
