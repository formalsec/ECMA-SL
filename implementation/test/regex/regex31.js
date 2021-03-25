/* Boundary assertions */

var r = /LO\B/;

var ret = r.exec("HELLO, LOOK AT YOU");

assert.sameValue(ret[0], "LO");
assert.sameValue(ret.index, 7);