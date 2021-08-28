// Copyright (C) 2015 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
es6id: 23.1.3.10
description: >
  Property type and descriptor.
info: |
  get Map.prototype.size

  17 ECMAScript Standard Built-in Objects

includes: [propertyHelper.js]
---*/

var descriptor = Object.getOwnPropertyDescriptor(Map.prototype, 'size');
console.log("js1")
assert.sameValue(
  typeof descriptor.get,
  'function',
  'typeof descriptor.get is function'
);
console.log("js2")
assert.sameValue(
  typeof descriptor.set,
  'undefined',
  'typeof descriptor.set is undefined'
);
console.log("js3")
verifyNotEnumerable(Map.prototype, 'size');
console.log("js4")
verifyConfigurable(Map.prototype, 'size');
