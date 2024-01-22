let obj = {};
console.log(obj.constructor.prototype.__proto__);
let proto = obj.__proto__;
console.log(proto);
// proto.foo = "bar";
// console.log(({}).foo);
