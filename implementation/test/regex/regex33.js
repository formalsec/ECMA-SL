/* non-capturing group */

var r = /(LO)\B/;

var ret = r.exec("HELLO, LOOK AT YOU");

assert.sameValue(ret[0], "LO");
assert.sameValue(ret[1], "LO");
assert.sameValue(ret.index, 7);
assert.sameValue(ret.length, 2);