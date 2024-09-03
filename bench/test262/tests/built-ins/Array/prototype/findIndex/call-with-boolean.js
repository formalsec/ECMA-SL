// Copyright (c) 2020 Rick Waldron.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-array.prototype.findIndex
description: Array.prototype.findIndex applied to boolean primitive
---*/

assert.sameValue(
  Array.prototype.findIndex.call(true, function () /* TODO: => */ {}),
  -1,
  'Array.prototype.findIndex.call(true, function () /* TODO: => */ {}) must return -1'
);
assert.sameValue(
  Array.prototype.findIndex.call(false, function () /* TODO: => */ {}),
  -1,
  'Array.prototype.findIndex.call(false, function () /* TODO: => */ {}) must return -1'
);
