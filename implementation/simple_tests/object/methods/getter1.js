// Basic getter

var o = { bar: 0 }

Object.defineProperty(o, "foo", { get: function () { return this.bar + 1; } });

console.log(o.foo)
