// Writable DataDescriptor

var o = { bar: 2 };

Object.defineProperty(o, 'key', {
  enumerable: false,
  configurable: false,
  writable: true,
  value: 'static'
});

o.key = 'not static';

console.log(o.key);
