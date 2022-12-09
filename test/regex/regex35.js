/* non-capturing group */

var r = /c+?a+c+/;

var ret = r.exec("ccaacc");

assert.sameValue(ret[0], ret.input);
assert.sameValue(ret.index, 0);
assert.sameValue(ret.length, 1);