// Basic Prototype Inheritance

var o = { bar: 2 };

var a = {}

Object.setPrototypeOf(a, o)

console.log(a.bar);
