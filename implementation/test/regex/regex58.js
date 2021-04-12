var re = /(b)/ig;
var ret = "abcb".match(re);

assert.sameValue(ret.length, 2);
assert.sameValue(ret[0], "b");
assert.sameValue(ret[1], "b");