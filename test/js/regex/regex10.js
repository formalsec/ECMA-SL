/* Repeat matcher */

var r = /c{2}/;
var ret = r.exec("ccaaaaacc");

assert.sameValue(ret[0], "cc");
assert.sameValue(ret.index, 0);