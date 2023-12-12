/* Repeat matcher */

var r = /a{1,}/;
var ret = r.exec("ccaaaaacc");

assert.sameValue(ret[0], "aaaaa");
assert.sameValue(ret.index, 2);