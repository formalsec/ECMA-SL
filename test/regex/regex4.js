var r = /(a|b)\1/;
var ret = r.exec("ccbbcc");

assert.sameValue(ret[0], "bb");
assert.sameValue(ret[1], "b");
assert.sameValue(ret.index, 2);
assert.sameValue(ret.length, 2);