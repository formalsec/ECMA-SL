var r = /\n\v\f\r\t\0/;
var ret = r.exec("ccab\n\v\f\r\t\0cc");

assert.sameValue(r.lastIndex, 0);
assert.sameValue(ret.length, 1);
assert.sameValue(ret[0], "\n\v\f\r\t\0");
assert.sameValue(ret.index, 4);
assert.sameValue(ret.input, "ccab\n\v\f\r\t\0cc");

r = /[\n]+/;
ret = r.exec("ccab\n\n\ncc");

assert.sameValue(r.lastIndex, 0);
assert.sameValue(ret.length, 1);
assert.sameValue(ret[0], "\n\n\n");
assert.sameValue(ret.index, 4);
assert.sameValue(ret.input, "ccab\n\n\ncc");