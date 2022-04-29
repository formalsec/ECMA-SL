// Copyright (c) 2020 Rick Waldron.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-array.prototype.forEach
description: Array.prototype.forEach applied to boolean primitive
---*/

assert.sameValue(
  Array.prototype.forEach.call(true, function () /* TODO: => */ {}),
  undefined,
  'Array.prototype.forEach.call(true, function () /* TODO: => */ {}) must return undefined'
);
assert.sameValue(
  Array.prototype.forEach.call(false, function () /* TODO: => */ {}),
  undefined,
  'Array.prototype.forEach.call(false, function () /* TODO: => */ {}) must return undefined'
);
