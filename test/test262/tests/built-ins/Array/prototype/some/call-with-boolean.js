// Copyright (c) 2020 Rick Waldron.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-array.prototype.some
description: Array.prototype.some applied to boolean primitive
---*/

assert.sameValue(
  Array.prototype.some.call(true, function () /* TODO: => */ {}),
  false,
  'Array.prototype.some.call(true, function () /* TODO: => */ {}) must return false'
);
assert.sameValue(
  Array.prototype.some.call(false, function () /* TODO: => */ {}),
  false,
  'Array.prototype.some.call(false, function () /* TODO: => */ {}) must return false'
);
