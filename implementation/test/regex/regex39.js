/* non-capturing group */

var r = /[abc]/;

var ret = r.exec("ccaabb");

assert.sameValue(ret[0], "c");
assert.sameValue(ret.index, 0);
assert.sameValue(ret.length, 1);