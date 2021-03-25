/* Repeat matcher */

var r = /a{1,3}/;
var ret = r.exec("ccaaaaacc");

assert.sameValue(ret[0], "aaa");
assert.sameValue(ret.index, 2);