/* Boundary assertions */

var r = /^a$/;

var ret = r.exec("a");

assert.sameValue(ret[0], "a");