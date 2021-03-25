/* Disjunction */

var r = /a|b/;
var ret = r.exec("ccbacc");

assert.sameValue(ret[0], "b");
assert.sameValue(ret.index, 2);