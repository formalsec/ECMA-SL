/* non-capturing group */

var r = /[a-z]/;

var ret = r.exec("Ccaabb");

assert.sameValue(ret[0], "c");
assert.sameValue(ret.index, 1);
assert.sameValue(ret.length, 1);