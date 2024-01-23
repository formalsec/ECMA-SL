let obj = {};
obj.__proto__.foo = "bar";
console.log(({}).foo);
let obj2 = { "__proto__" : { bar : "baz" } };
console.log(({}).bar);
console.log(obj2.bar);
let obj3 = { ["__proto__"] : { baz : "qux" } };
console.log(obj3.baz);
console.log(obj3.__proto__.baz);
