var r = /[.]/;

var ret = r.exec(".");

assert.sameValue(r.lastIndex, 0);
assert.sameValue(ret.length, 1);
assert.sameValue(ret[0], ".");
assert.sameValue(ret.index, 0);

ret = r.exec("m");

assert.sameValue(ret, null);