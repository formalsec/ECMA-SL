// Basic Prototype Inheritance

var o = { bar: 2 };

var a = {}

Object.setPrototypeOf(a, o)

var b = Object.getPrototypeOf(a)

console.logObject(b)
