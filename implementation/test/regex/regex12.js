/* Repeat matcher */

var r = /c{2}/gim;
var ret = r.exec("ccaaaaacc");

console.log(r.lastIndex);
console.log(ret[0]);

assert.sameValue(r.lastIndex, 2);
assert.sameValue(ret[0], "cc");
assert.sameValue(ret.index, 0);