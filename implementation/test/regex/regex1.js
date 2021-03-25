var r = /ab/;
var ret = r.exec("ccabcc");

assert.sameValue(r.lastIndex, 0);
assert.sameValue(ret.length, 1);
assert.sameValue(ret[0], "ab");
assert.sameValue(ret.index, 2);
assert.sameValue(ret.input, "ccabcc");