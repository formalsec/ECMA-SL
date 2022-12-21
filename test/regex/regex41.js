var r = /.+/;
var ret = r.exec("ccabcc");

assert.sameValue(r.lastIndex, 0);
assert.sameValue(ret.length, 1);
assert.sameValue(ret[0], "ccabcc");
assert.sameValue(ret.index, 0);
assert.sameValue(ret.input, "ccabcc");
