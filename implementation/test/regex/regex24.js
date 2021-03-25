/* Boundary assertions */

var r = /^a$/m;

var ret = r.exec("a");

assert.sameValue(ret[0], "a");