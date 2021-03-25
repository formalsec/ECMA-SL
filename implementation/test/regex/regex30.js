/* Boundary assertions */

var r = /\BLO/;

var ret = r.exec("HELLO, LOOK AT YOU");

assert.sameValue(ret[0], "LO");
assert.sameValue(ret.index, 3);