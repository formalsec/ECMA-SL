// Copyright (C) 2014 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
es6id: 21.1.5.2
description: >
    The [[Prototype]] internal slot ofthe %StringIteratorPrototype% is the
    %IteratorPrototype% intrinsic object (25.1.2).
features: [Symbol.iterator]
---*/
console.log(Object.getPrototypeOf(''[Symbol.iterator]()));

var strItrProto = Object.getPrototypeOf(''[Symbol.iterator]());

console.log("0");

var arrItrProto = [][Symbol.iterator];

console.log( "0.1");
console.log(arrItrProto);

var itrProto = Object.getPrototypeOf(
  Object.getPrototypeOf([][Symbol.iterator]())
);

console.log(Object.getPrototypeOf(strItrProto));

console.log("1");

console.log(itrProto);

console.log("2");
assert.sameValue(Object.getPrototypeOf(strItrProto), itrProto);
