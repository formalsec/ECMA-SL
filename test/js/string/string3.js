var s1 = String(100000000000000000000);
var s2 = String(100000000000000000000.123);

console.log(s1);
console.log(s2);

assert.sameValue(s1, '100000000000000000000');
assert.sameValue(s2, '100000000000000000000.123');
