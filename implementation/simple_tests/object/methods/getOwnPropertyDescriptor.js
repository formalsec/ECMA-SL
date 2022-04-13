o = {};
Object.defineProperty(o, 'baz', { value: 8675309, writable: false, enumerable: false });
d = Object.getOwnPropertyDescriptor(o, 'baz');

console.log(d)
