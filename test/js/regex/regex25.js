/* Boundary assertions */

var r = /^a$/m;

var ret = r.exec("ab");

assert.sameValue(ret, null);