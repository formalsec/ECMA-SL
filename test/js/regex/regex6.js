/* Repeat matcher */

var r = /a*/;
var ret = r.exec("aaacc");

assert.sameValue(ret[0], "aaa");
assert.sameValue(ret.index, 0);