// Basic Prototype Inheritance

var o = { bar: 2 };

var a = {}

Object.setPrototypeOf(a, o)

var isProto = o.isPrototypeOf(a)

console.log(isProto)
