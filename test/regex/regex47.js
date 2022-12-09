var r = /(?=(.*b))(a)(b)/;

var ret = r.exec("abc");

assert.sameValue(r.lastIndex, 0);
assert.sameValue(ret.length, 4);
assert.sameValue(ret[0], "ab");
assert.sameValue(ret.index, 0);
