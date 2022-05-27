// Copyright 2020 Rick Waldron.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-number.prototype.tostring
description: >
  If radixNumber < 2 or radixNumber > 36, throw a RangeError exception.
---*/

assert.throws(RangeError, function () /* TODO: => */ {
  0..toString(37);
});
assert.throws(RangeError, function () /* TODO: => */ {
  1..toString(37);
});
assert.throws(RangeError, function () /* TODO: => */ {
  NaN.toString(37);
});
assert.throws(RangeError, function () /* TODO: => */ {
  Infinity.toString(37);
});
