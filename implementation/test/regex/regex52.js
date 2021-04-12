var r = /\0+/;

var ret = r.exec("\0\0");
console.log(ret);
assert.sameValue(ret[0], "\0\0");
assert.sameValue(ret.index, 0);

ret = r.exec("abc\0");
assert.sameValue(ret[0], "\0");
assert.sameValue(ret.index, 3);