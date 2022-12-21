/* non-capturing group */

var r = /a*?/;

var ret = r.exec("ccaacc");

assert.sameValue(ret[0], "");
assert.sameValue(ret.index, 0);
assert.sameValue(ret.length, 1);