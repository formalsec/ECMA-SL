/* Positive look-ahead */

var r = /aaa(?!aa)/;

var ret = r.exec("aaaaa");

assert.sameValue(ret[0], "aaa");
assert.sameValue(ret.index, 1);