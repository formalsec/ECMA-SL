// Object assign

var target = { a: 1, b: 2 };
var source = { b: 4, c: 5 };

var returnedTarget = Object.assign(target, source);

console.logObject(target);
// expected output: Object { a: 1, b: 4, c: 5 }

console.logObject(returnedTarget);
// expected output: Object { a: 1, b: 4, c: 5 }
