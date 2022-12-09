/* Repeat matcher */

var r = /a+/;
var ret = r.exec("ccaaacc");

assert.sameValue(ret[0], "aaa");
assert.sameValue(ret.index, 2);