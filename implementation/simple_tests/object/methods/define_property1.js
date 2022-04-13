
// Basic Object creation and property assignment with property descriptors

var o = { bar: 2 };

Object.defineProperty(o, 'key', {
  enumerable: false,
  configurable: false,
  writable: false,
  value: 'static'
});

o.key = 'not static'

console.log(o.key)
