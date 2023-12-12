/* Backreference */

var r = /(ab)\1/;
var ret = r.exec("ccababcc");

assert.sameValue(ret[0], "abab");
assert.sameValue(ret.index, 2);