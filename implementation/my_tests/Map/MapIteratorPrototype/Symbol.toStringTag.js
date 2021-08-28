// Copyright (C) 2014 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
es6id: 23.1.5.2.2
description: >
   `Symbol.toStringTag` property descriptor
info: |
    The initial value of the @@toStringTag property is the String value
    "Map Iterator".

    This property has the attributes { [[Writable]]: false, [[Enumerable]]:
    false, [[Configurable]]: true }.
includes: [propertyHelper.js]
features: [Symbol.iterator, Symbol.toStringTag]
---*/

var MapIteratorProto = Object.getPrototypeOf(new Map()[Symbol.iterator]());
console.log("js1")
assert.sameValue('Map Iterator', MapIteratorProto[Symbol.toStringTag]);
console.log("js2")
verifyNotEnumerable(MapIteratorProto, Symbol.toStringTag);
console.log("js3")
verifyNotWritable(MapIteratorProto, Symbol.toStringTag);
console.log("js4")
verifyConfigurable(MapIteratorProto, Symbol.toStringTag);
console.log("js5")