var obj1 = Object.create(null);

var obj2 = o = Object.create(Object.prototype, {
  foo: { writable: true, configurable: true, value: 'hello' },
  bar: {
    configurable: false,
    get function() { return 10; },
    set function(value) { console.log('Setting `o.bar` to', value); }
  }
});

console.logObject(obj1)
console.logObject(obj2)
